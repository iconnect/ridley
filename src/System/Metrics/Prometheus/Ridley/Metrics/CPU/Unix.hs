{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Metrics.Prometheus.Ridley.Metrics.CPU.Unix
  ( getLoadAvg
  , processCPULoad
  ) where

import qualified Data.Text as T
import           Data.Traversable
import qualified Data.Vector as V
import           Shelly
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import           System.Metrics.Prometheus.Ridley.Types
import           Text.Read (readMaybe)

--------------------------------------------------------------------------------
getLoadAvg :: IO (V.Vector Double)
getLoadAvg = do
  rawOutput <- shelly $ silently $ take 3 . T.lines . T.strip <$> run "cat" ["/proc/loadavg"]
  let loads = case traverse (readMaybe . T.unpack) rawOutput of
                Just [a,b,c] -> [a,b,c]
                _            -> [-1.0, -1.0, -1.0]
  return $ V.fromList loads

--------------------------------------------------------------------------------
-- | As we have 3 gauges, it makes no sense flushing them.
updateCPULoad :: (P.Gauge, P.Gauge, P.Gauge) -> Bool -> IO ()
updateCPULoad (cpu1m, cpu5m, cpu15m) _ = do
  loadVec <- getLoadAvg
  P.set (loadVec `V.unsafeIndex` 0) cpu1m
  P.set (loadVec `V.unsafeIndex` 1) cpu5m
  P.set (loadVec `V.unsafeIndex` 2) cpu15m

--------------------------------------------------------------------------------
processCPULoad :: (P.Gauge, P.Gauge, P.Gauge) -> RidleyMetricHandler
processCPULoad g = RidleyMetricHandler {
    metric = g
  , updateMetric = updateCPULoad
  , flush = False
  }
