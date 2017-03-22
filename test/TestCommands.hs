{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Foreign.C.String
import System.Metrics.Prometheus.Ridley.Metrics.CPU
import System.Metrics.Prometheus.Ridley.Metrics.Network

doWork :: IO ()
#if defined darwin_HOST_OS
doWork = do
  (ifaces, dtor)  <- getNetworkMetrics
  forM_ ifaces $ \d@IfData{..} -> do
    s <- peekCAString ifi_name
    print d
    print s
  dtor
#else
doWork = do
  ifaces  <- getNetworkMetrics
  forM_ ifaces print
#endif

main :: IO ()
main = do
  print "getLoadAvg"
  r <- getLoadAvg
  print r
  print "getNetworkMetrics"
  doWork
  print "done"
