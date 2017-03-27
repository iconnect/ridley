{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Ridley.Metrics.Network
  ( networkMetrics
  , getNetworkMetrics
  , mkInterfaceGauge
  , NetworkMetrics
  , NetworkMetric
  , IfData(..)
  ) where

#ifdef darwin_HOST_OS
import System.Metrics.Prometheus.Ridley.Metrics.Network.Darwin
#else
import System.Metrics.Prometheus.Ridley.Metrics.Network.Unix
#endif

import System.Metrics.Prometheus.Ridley.Metrics.Network.Types
