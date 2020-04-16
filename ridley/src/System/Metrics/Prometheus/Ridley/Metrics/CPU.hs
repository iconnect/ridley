{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Ridley.Metrics.CPU
  ( getLoadAvg
  , processCPULoad
  ) where

#ifdef darwin_HOST_OS
import           System.Metrics.Prometheus.Ridley.Metrics.CPU.Darwin
#else
import           System.Metrics.Prometheus.Ridley.Metrics.CPU.Unix
#endif
