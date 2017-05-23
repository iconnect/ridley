{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Metrics.Prometheus.Ridley.Metrics.CPU.Unix
  ( getLoadAvg
  , processCPULoad
  ) where

import           Control.Applicative ((<|>))
import           Data.Maybe (fromJust)
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
  rawOutput <- shelly $ silently $ T.strip <$> run "cat" ["/proc/loadavg"]
  let standardFormat = case traverse (readMaybe . T.unpack) (take 3 . T.lines $ rawOutput) of
                         Just [a,b,c] -> Just [a,b,c]
                         _            -> Nothing

  -- See: https://github.com/iconnect/ridley/issues/8
  let alternativeFormat = case traverse (readMaybe . T.unpack) (take 3 . T.splitOn " " $ rawOutput) of
                         Just [a,b,c] -> Just [a,b,c]
                         _            -> Nothing

  return . V.fromList . fromJust $ standardFormat <|> alternativeFormat <|> Just noAvgInfo
  where
    noAvgInfo = [-1.0, -1.0, -1.0]

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
