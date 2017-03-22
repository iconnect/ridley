{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Ridley.Metrics.CPU
  ( getLoadAvg
  , processCPULoad
  ) where

import           Data.Monoid ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import qualified Language.C.Inline as C
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import           System.Metrics.Prometheus.Ridley.Types

C.context (C.baseCtx <> C.vecCtx)
C.include "<stdlib.h>"

--------------------------------------------------------------------------------
getLoadAvg :: IO (V.Vector CDouble)
getLoadAvg = do
  v <- VM.new 3
  _ <- [C.exp| int { getloadavg($vec-ptr:(double* v), 3) } |]
  V.freeze v

--------------------------------------------------------------------------------
-- | As we have 3 gauges, it makes no sense flushing them.
updateCPULoad :: (P.Gauge, P.Gauge, P.Gauge) -> Bool -> IO ()
updateCPULoad (cpu1m, cpu5m, cpu15m) _ = do
  loadVec <- getLoadAvg
  P.set (fromCDouble $ loadVec `V.unsafeIndex` 0) cpu1m
  P.set (fromCDouble $ loadVec `V.unsafeIndex` 1) cpu5m
  P.set (fromCDouble $ loadVec `V.unsafeIndex` 2) cpu15m
  where
    fromCDouble :: CDouble -> Double
    fromCDouble (CDouble d) = d

--------------------------------------------------------------------------------
processCPULoad :: (P.Gauge, P.Gauge, P.Gauge) -> RidleyMetricHandler
processCPULoad g = RidleyMetricHandler {
    metric = g
  , updateMetric = updateCPULoad
  , flush = False
  }
