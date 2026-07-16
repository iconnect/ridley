{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Mirrors an EKG 'EKG.Store' into a concurrent Prometheus registry.
--
-- This replaces @System.Remote.Monitoring.Prometheus.registerEKGStore@ from
-- @ekg-prometheus-adapter@, which is hardwired to the 'StateT'-based registry
-- and cannot be used now that Ridley runs on the concurrent one.
module System.Metrics.Prometheus.Ridley.EKG (
    registerEKGStore
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Lens.Micro
import qualified System.Metrics as EKG
import qualified System.Metrics.Prometheus.Concurrent.Registry as PC
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.MetricId as Prometheus
import           System.Remote.Monitoring.Prometheus (AdapterOptions(..), labels, namespace, samplingFrequency)

--------------------------------------------------------------------------------
data Metric =
    C Counter.Counter
  | G Gauge.Gauge

type MetricsMap = Map.Map Prometheus.Name Metric

--------------------------------------------------------------------------------
-- | Registers all the metrics of the input EKG store into the given registry
-- and forks a thread which keeps them up to date, sampling the EKG store
-- every 'samplingFrequency' seconds.
registerEKGStore :: MonadIO m => EKG.Store -> AdapterOptions -> PC.Registry -> m ()
registerEKGStore store opts registry = liftIO $ do
  samples <- EKG.sampleAll store
  mmap <- foldM (mkMetric opts registry) Map.empty (HMap.toList samples)
  void $ forkIO $ forever $ do
    threadDelay (opts ^. samplingFrequency * 10^(6 :: Int))
    updateMetrics store opts mmap

--------------------------------------------------------------------------------
mkMetric :: AdapterOptions -> PC.Registry -> MetricsMap -> (T.Text, EKG.Value) -> IO MetricsMap
mkMetric opts registry mmap (key, value) = do
  let k = mkKey opts key
  case value of
   EKG.Counter c -> do
     counter <- PC.registerCounter k (opts ^. labels) registry
     Counter.add (fromIntegral c) counter
     return $! Map.insert k (C counter) mmap
   EKG.Gauge g   -> do
     gauge <- PC.registerGauge k (opts ^. labels) registry
     Gauge.set (fromIntegral g) gauge
     return $! Map.insert k (G gauge) mmap
   EKG.Label _   -> return mmap
   EKG.Distribution _ -> return mmap

--------------------------------------------------------------------------------
updateMetrics :: EKG.Store -> AdapterOptions -> MetricsMap -> IO ()
updateMetrics store opts mmap = do
  samples <- EKG.sampleAll store
  forM_ (HMap.toList samples) (updateMetric opts mmap)

--------------------------------------------------------------------------------
mkKey :: AdapterOptions -> T.Text -> Prometheus.Name
mkKey opts k =
  Prometheus.Name $ maybe mempty (<> "_") (opts ^. namespace) <> T.replace "." "_" k

--------------------------------------------------------------------------------
updateMetric :: AdapterOptions -> MetricsMap -> (T.Text, EKG.Value) -> IO ()
updateMetric opts mmap (key, value) = do
  let k = mkKey opts key
  case (Map.lookup k mmap, value) of
    (Just (C counter), EKG.Counter c)  -> do
      (Counter.CounterSample oldCounterValue) <- Counter.sample counter
      let slack = c - fromIntegral oldCounterValue
      when (slack >= 0) $ Counter.add (fromIntegral slack) counter
    (Just (G gauge), EKG.Gauge g) ->
      Gauge.set (fromIntegral g) gauge
    _ -> return ()
