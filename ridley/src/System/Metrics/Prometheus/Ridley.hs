{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
import           Data.IORef
import qualified Data.List as List
import           Data.Map.Strict as M
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           GHC.Conc (getNumCapabilities, getNumProcessors)
import           Katip
import           Lens.Micro
import           Network.Wai.Metrics (registerWaiMetrics)
import           System.Metrics as EKG
import qualified System.Metrics.Prometheus.Http.Scrape as P
import           System.Metrics.Prometheus.Metric.Counter (add)
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Metrics.CPU
import           System.Metrics.Prometheus.Ridley.Metrics.DiskUsage
import           System.Metrics.Prometheus.Ridley.Metrics.Memory
import           System.Metrics.Prometheus.Ridley.Metrics.Network
import           System.Metrics.Prometheus.Ridley.Types
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
registerMetrics :: [RidleyMetric] -> Ridley [RidleyMetricHandler]
registerMetrics [] = return []
registerMetrics (x:xs) = do
  opts <- ask
  let popts = opts ^. prometheusOptions
  let sev   = opts ^. katipSeverity
  case x of
    CustomMetric metricName custom -> do
      customMetric <- lift (custom opts)
      $(logTM) sev $ "Registering CustomMetric '" <> fromString (T.unpack metricName) <> "'..."
      (customMetric :) <$> (registerMetrics xs)
    ProcessMemory -> do
      processReservedMemory <- lift $ P.registerGauge "process_memory_kb" (popts ^. labels)
      let !m = processMemory processReservedMemory
      $(logTM) sev "Registering ProcessMemory metric..."
      (m :) <$> (registerMetrics xs)
    CPULoad -> do
      cpu1m  <- lift $ P.registerGauge "cpu_load1"  (popts ^. labels)
      cpu5m  <- lift $ P.registerGauge "cpu_load5"  (popts ^. labels)
      cpu15m <- lift $ P.registerGauge "cpu_load15" (popts ^. labels)
      let !cpu = processCPULoad (cpu1m, cpu5m, cpu15m)
      $(logTM) sev "Registering CPULoad metric..."
      (cpu :) <$> (registerMetrics xs)
    GHCConc -> do
      -- We don't want to keep updating this as it's a one-shot measure.
      numCaps  <- lift $ P.registerCounter "ghc_conc_num_capabilities"  (popts ^. labels)
      numPros  <- lift $ P.registerCounter "ghc_conc_num_processors"    (popts ^. labels)
      liftIO (getNumCapabilities >>= \cap -> add (fromIntegral cap) numCaps)
      liftIO (getNumProcessors >>= \cap -> add (fromIntegral cap) numPros)
      $(logTM) sev "Registering GHCConc metric..."
      registerMetrics xs
    -- Ignore `Wai` as we will use an external library for that.
    Wai     -> registerMetrics xs
    DiskUsage -> do
      diskStats <- liftIO getDiskStats
      dmap   <- lift $ foldM (mkDiskGauge (popts ^. labels)) M.empty diskStats
      let !diskUsage = diskUsageMetrics dmap
      $(logTM) sev "Registering DiskUsage metric..."
      (diskUsage :) <$> registerMetrics xs
    Network -> do
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
      (network :) <$> registerMetrics xs

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
      let f mle0 (n, s) = do
            le0 <- mle0
            registerScribe n s defaultScribeSettings le0
      le' <- List.foldl' f (return le) (opts ^. katipScribes . _2)

      -- Start the server
      serverLoop <- async $ runRidley opts le' $ do
        lift $ registerEKGStore store (opts ^. prometheusOptions)
        handlers <- registerMetrics (Set.toList $ opts ^. ridleyMetrics)

        liftIO $ do
          lastUpdate <- newIORef =<< getCurrentTime
          updateLoop <- async $ handlersLoop lastUpdate handlers
          putMVar x updateLoop

        lift $ P.sample >>= P.serveHttpTextMetrics port path

      ul  <- takeMVar x
      link2 serverLoop ul
      res <- waitCatch ul
      case res of
        Left e  -> runKatipContextT le' () "errors" $ do
          $(logTM) ErrorS (fromString $ show e)
        Right _ -> return ()

    handlersLoop :: IORef UTCTime -> [RidleyMetricHandler] -> IO a
    handlersLoop lastUpdateRef handlers = do
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
      updateHandlers (List.map (\x -> x { flush = mustFlush }) handlers)
      handlersLoop lastUpdateRef handlers

--------------------------------------------------------------------------------
updateHandlers :: [RidleyMetricHandler] -> IO ()
updateHandlers = mapM_ runHandler
