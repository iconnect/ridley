{-# LANGUAGE OverloadedStrings #-}
module System.Metrics.Prometheus.Ridley.Metrics.Memory (
    processMemory
  ) where

import qualified System.Metrics.Prometheus.Metric.Gauge as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Process
import           System.Process
import           Text.Read

--------------------------------------------------------------------------------
-- | Return the amount of occupied memory for this
-- process. We use unix's `ps` command that,
-- although has the reputation of not being 100%
-- accurate, at least works on Darwin and Linux
-- without using any CPP processor.
-- Returns the memory in Kb.
getProcessMemory :: IO (Maybe Integer)
getProcessMemory = do
  myPid <- getProcessID
  readMaybe <$> readProcess "ps" ["-o", "rss=", "-p", show myPid] []

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
updateProcessMemory :: P.Gauge -> Bool -> IO ()
updateProcessMemory g _ = do
  mbMem <- getProcessMemory
  case mbMem of
    Nothing -> return ()
    Just m  -> P.set (fromIntegral m) g

--------------------------------------------------------------------------------
processMemory :: P.Gauge -> RidleyMetricHandler
processMemory g = mkRidleyMetricHandler "ridley-process-memory" g updateProcessMemory False
