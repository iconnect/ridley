{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Scott.Metrics.Network
  ( networkMetrics
  , getNetworkMetrics
  , mkInterfaceGauge
  , NetworkMetrics
  , NetworkMetric
  , IfData(..)
  ) where

#if defined darwin_HOST_OS
import System.Metrics.Prometheus.Scott.Metrics.Network.MacOSX
#else
import System.Metrics.Prometheus.Scott.Metrics.Network.Unix
#endif

import System.Metrics.Prometheus.Scott.Metrics.Network.Types
