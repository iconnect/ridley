{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Ridley (
    startRidley
  , startRidleyWithStore
  -- * Handy re-exports
  , prometheusOptions
  , ridleyMetrics
  , AdapterOptions(..)
  , RidleyCtx
  , ridleyWaiMetrics
  , ridleyThreadId
  , katipScribes
  , dataRetentionPeriod
  , samplingFrequency
  , namespace
  , labels
  , newOptions
  , defaultMetrics
  ) where

import           Control.AutoUpdate as Auto
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import qualified Control.Exception.Safe as Ex
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (asks)
import           Control.Monad.Trans.Class (lift)
import           Data.IORef
import qualified Data.List as List
import           Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           GHC.Conc (getNumCapabilities, getNumProcessors)
import           GHC.Stack
import           Katip
import           Lens.Micro
import           Lens.Micro.Extras (view)
import           Network.Wai.Metrics (registerWaiMetrics)
import           System.Metrics as EKG
#if (MIN_VERSION_prometheus(0,5,0))
import qualified System.Metrics.Prometheus.Http.Scrape as P
#else
import qualified System.Metrics.Prometheus.Concurrent.Http as P
#endif
import           System.Metrics.Prometheus.Metric.Counter (add)
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Registry (RegistrySample)
import           System.Metrics.Prometheus.Ridley.Metrics.CPU
import           System.Metrics.Prometheus.Ridley.Metrics.DiskUsage
import           System.Metrics.Prometheus.Ridley.Metrics.Memory
import           System.Metrics.Prometheus.Ridley.Metrics.Network
import           System.Metrics.Prometheus.Ridley.Types
import           System.Metrics.Prometheus.Ridley.Types.Internal
import           System.Remote.Monitoring.Prometheus

--------------------------------------------------------------------------------
startRidley :: RidleyOptions
            -> P.Path
            -> Port
            -> IO RidleyCtx
startRidley opts path port = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  startRidleyWithStore opts path port store

--------------------------------------------------------------------------------
registerMetrics :: Set.Set RidleyMetric -> Ridley [RidleyMetricHandler]
registerMetrics = foldM registerSingleMetric []
  where
    registerSingleMetric :: [RidleyMetricHandler] -> RidleyMetric -> Ridley [RidleyMetricHandler]
    registerSingleMetric !acc x = case x of
        CustomMetric metricName mb_timeout custom
          -> tryRegister x acc $ registerCustomMetric metricName mb_timeout custom
        ProcessMemory
          -> tryRegister x acc registerProcessMemory
        CPULoad
          -> tryRegister x acc registerCPULoad
        GHCConc
          -> tryRegister x acc registerGHCConc
        Wai
          -> pure acc -- Ignore `Wai` as we will use an external library for that.
        DiskUsage
          -> tryRegister x acc registerDiskUsage
        Network
          -> tryRegister x acc registerNetworkMetric

tryRegister :: RidleyMetric -> [RidleyMetricHandler] -> Ridley RidleyMetricHandler -> Ridley [RidleyMetricHandler]
tryRegister metric !acc doRegister = do
  registrationResult <- Ex.tryAny doRegister
  case registrationResult of
    Left ex -> do
      $(logTM) ErrorS $ ls $ T.pack $ "Registration of metric '" <> show metric <> "' failed due to: " <> show ex
      pure acc
    Right metricHandler -> pure $! metricHandler : acc

registerProcessMemory :: Ridley RidleyMetricHandler
registerProcessMemory = do
  sev   <- asks (view katipSeverity)
  popts <- asks (view prometheusOptions)
  logger <- ioLogger
  processReservedMemory <- lift $ P.registerGauge "process_memory_kb" (popts ^. labels)
  let !m = processMemory logger processReservedMemory
  $(logTM) sev "Registering ProcessMemory metric..."
  pure m

registerCPULoad :: Ridley RidleyMetricHandler
registerCPULoad = do
  sev   <- asks (view katipSeverity)
  popts <- asks (view prometheusOptions)
  cpu1m  <- lift $ P.registerGauge "cpu_load1"  (popts ^. labels)
  cpu5m  <- lift $ P.registerGauge "cpu_load5"  (popts ^. labels)
  cpu15m <- lift $ P.registerGauge "cpu_load15" (popts ^. labels)
  let !cpu = processCPULoad (cpu1m, cpu5m, cpu15m)
  $(logTM) sev "Registering CPULoad metric..."
  pure cpu

registerGHCConc :: Ridley RidleyMetricHandler
registerGHCConc = do
  sev   <- asks (view katipSeverity)
  popts <- asks (view prometheusOptions)
  -- We don't want to keep updating this as it's a one-shot measure.
  numCaps  <- lift $ P.registerCounter "ghc_conc_num_capabilities"  (popts ^. labels)
  numPros  <- lift $ P.registerCounter "ghc_conc_num_processors"    (popts ^. labels)
  liftIO (getNumCapabilities >>= \cap -> add (fromIntegral cap) numCaps)
  liftIO (getNumProcessors >>= \cap -> add (fromIntegral cap) numPros)
  $(logTM) sev "Registering GHCConc metric..."
  pure $ mkRidleyMetricHandler "ridley-ghc-conc" (numCaps, numPros) noUpdate False

registerDiskUsage :: Ridley RidleyMetricHandler
registerDiskUsage = do
  sev   <- asks (view katipSeverity)
  diskUsage <- newDiskUsageMetrics
  $(logTM) sev "Registering DiskUsage metric..."
  pure diskUsage

registerCustomMetric :: T.Text -> Maybe Int -> Ridley RidleyMetricHandler -> Ridley RidleyMetricHandler
registerCustomMetric metricName mb_timeout custom = do
  opts    <- getRidleyOptions
  let sev = opts ^. katipSeverity
  le      <- getLogEnv
  customMetric <- case mb_timeout of
    Nothing   -> custom
    Just microseconds -> do
      RidleyMetricHandler mtr upd flsh lbl cs <- custom
      doUpdate <- liftIO $ Auto.mkAutoUpdate Auto.defaultUpdateSettings
                    { updateAction = upd mtr flsh `Ex.catch` logFailedUpdate le lbl cs
                    , updateFreq   = microseconds
                    }
      pure $ RidleyMetricHandler mtr (\_ _ -> doUpdate) flsh lbl cs
  $(logTM) sev $ "Registering CustomMetric '" <> fromString (T.unpack metricName) <> "'..."
  pure customMetric

registerNetworkMetric :: Ridley RidleyMetricHandler
registerNetworkMetric = do
  sev   <- asks (view katipSeverity)
  popts <- asks (view prometheusOptions)
#if defined darwin_HOST_OS
  (ifaces, dtor) <- liftIO getNetworkMetrics
  imap   <- lift $ foldM (mkInterfaceGauge (popts ^. labels)) M.empty ifaces
  liftIO dtor
#else
  ifaces <- liftIO getNetworkMetrics
  imap   <- lift $ foldM (mkInterfaceGauge (popts ^. labels)) M.empty ifaces
#endif
  let !network = networkMetrics imap
  $(logTM) sev "Registering Network metric..."
  pure network

--------------------------------------------------------------------------------
startRidleyWithStore :: RidleyOptions
                     -> P.Path
                     -> Port
                     -> EKG.Store
                     -> IO RidleyCtx
startRidleyWithStore opts path port store = do
  tid <- forkRidley
  mbMetr   <- case Set.member Wai (opts ^. ridleyMetrics) of
    False -> return Nothing
    True  -> Just <$> registerWaiMetrics store

  return $ RidleyCtx tid mbMetr
  where
    forkRidley = forkIO $ do
      x <- newEmptyMVar
      le <- initLogEnv (opts ^. katipScribes . _1) "production"

      -- Register all the externally-passed Katip's Scribe
#if (MIN_VERSION_katip(0,5,0))
      le' <- foldM (\le0 (n,s) -> registerScribe n s defaultScribeSettings le0) le (opts ^. katipScribes . _2)
#else
      let le' = List.foldl' (\le0 (n,s) -> registerScribe n s le0) le (opts ^. katipScribes . _2)
#endif

      -- Start the server
      serverLoop <- async $ runRidley opts le' $ do
        lift $ registerEKGStore store (opts ^. prometheusOptions)
        handlers <- registerMetrics (opts ^. ridleyMetrics)

        liftIO $ do
          lastUpdate <- newIORef =<< getCurrentTime
          updateLoop <- async $ handlersLoop le' lastUpdate handlers
          putMVar x updateLoop

        lift $ P.sample >>= serveMetrics port path

      ul  <- takeMVar x
      link2 serverLoop ul
      res <- waitCatch ul
      case res of
        Left e  -> runKatipContextT le' () "errors" $ do
          $(logTM) ErrorS (fromString $ show e)
        Right _ -> return ()

    handlersLoop :: LogEnv -> IORef UTCTime -> [RidleyMetricHandler] -> IO a
    handlersLoop le lastUpdateRef handlers = do
      let freq = opts ^. prometheusOptions . samplingFrequency
      let flushPeriod = opts ^. dataRetentionPeriod
      mustFlush <- case flushPeriod of
        Nothing -> return False
        Just p  -> do
          now        <- getCurrentTime
          lastUpdate <- readIORef lastUpdateRef
          case diffUTCTime lastUpdate now >= p of
            True  -> do
              modifyIORef' lastUpdateRef (const now)
              return True
            False -> return False
      threadDelay (freq * 10^6)
      updateHandlers le (List.map (\x -> x { flush = mustFlush }) handlers)
      handlersLoop le lastUpdateRef handlers

serveMetrics :: MonadIO m => Int -> P.Path -> IO RegistrySample -> m ()
#if (MIN_VERSION_prometheus(2,2,2))
serveMetrics = P.serveMetrics
#else
serveMetrics = P.serveHttpTextMetrics
#endif

--------------------------------------------------------------------------------
updateHandlers :: LogEnv -> [RidleyMetricHandler] -> IO ()
updateHandlers le hs = mapM_ (\h@RidleyMetricHandler{..} -> runHandler h `Ex.catchAny` (logFailedUpdate le label _cs)) hs

logFailedUpdate :: LogEnv -> T.Text -> CallStack -> Ex.SomeException -> IO ()
logFailedUpdate le lbl cs ex =
  runKatipContextT le () "errors" $ do
      $(logTM) ErrorS $
        fromString $ "Couldn't update handler for "
                  <> "\"" <> T.unpack lbl <> "\""
                  <> " due to "
                  <> Ex.displayException ex
                  <> " originally defined at "
                  <> prettyCallStack cs
