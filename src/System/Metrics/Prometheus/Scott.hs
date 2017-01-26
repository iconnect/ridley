{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Scott (
    startScott
  , startScottWithStore
  -- * Handy re-exports
  , prometheusOptions
  , scottMetrics
  , AdapterOptions(..)
  , ScottCtx
  , scottWaiMetrics
  , scottThreadId
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
import qualified Data.Set as Set
import           Data.String
import           Data.Time
import           GHC.Conc (getNumCapabilities, getNumProcessors)
import           Katip
import           Lens.Micro
import           Network.Wai.Metrics (registerWaiMetrics)
import           System.Metrics as EKG
import qualified System.Metrics.Prometheus.Concurrent.Http as P
import           System.Metrics.Prometheus.Metric.Counter (add)
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Scott.Metrics.CPU
import           System.Metrics.Prometheus.Scott.Metrics.DiskUsage
import           System.Metrics.Prometheus.Scott.Metrics.Memory
import           System.Metrics.Prometheus.Scott.Metrics.Network
import           System.Metrics.Prometheus.Scott.Types
import           System.Remote.Monitoring.Prometheus

--------------------------------------------------------------------------------
startScott :: ScottOptions
           -> P.Path
           -> Port
           -> IO ScottCtx
startScott opts path port = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  startScottWithStore opts path port store

--------------------------------------------------------------------------------
registerMetrics :: [ScottMetric] -> Scott [ScottMetricHandler]
registerMetrics [] = return []
registerMetrics (x:xs) = do
  opts <- ask
  let popts = opts ^. prometheusOptions
  let sev   = opts ^. katipSeverity
  case x of
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
startScottWithStore :: ScottOptions
                    -> P.Path
                    -> Port
                    -> EKG.Store
                    -> IO ScottCtx
startScottWithStore opts path port store = do
  tid <- forkScott
  mbMetr   <- case Set.member Wai (opts ^. scottMetrics) of
    False -> return Nothing
    True  -> Just <$> registerWaiMetrics store

  return $ ScottCtx tid mbMetr
  where
    forkScott = forkIO $ do
      x <- newEmptyMVar
      le <- initLogEnv (opts ^. katipScribes . _1) "production"

      -- Register all the externally-passed Katip's Scribe
      let le' = List.foldl' (\le0 (n,s) -> registerScribe n s le0) le (opts ^. katipScribes . _2)

      -- Start the server
      serverLoop <- async $ runScott opts le' $ do
        lift $ registerEKGStore store (opts ^. prometheusOptions)
        handlers <- registerMetrics (Set.toList $ opts ^. scottMetrics)

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

    handlersLoop :: IORef UTCTime -> [ScottMetricHandler] -> IO a
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
updateHandlers :: [ScottMetricHandler] -> IO ()
updateHandlers = mapM_ runHandler
