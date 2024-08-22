{-# LANGUAGE OverloadedStrings #-}
module System.Metrics.Prometheus.Ridley.Metrics.Memory (
    processMemory
  ) where

import           Katip
import           System.Exit
import           System.Metrics.Prometheus.Ridley.Types
import           System.Metrics.Prometheus.Ridley.Types.Internal
import           System.Posix.Process
import           System.Process
import           Text.Read
import qualified Data.Text as T
import qualified System.Metrics.Prometheus.Metric.Gauge as P

--------------------------------------------------------------------------------
-- | Return the amount of occupied memory for this
-- process. We use unix's `ps` command that,
-- although has the reputation of not being 100%
-- accurate, at least works on Darwin and Linux
-- without using any CPP processor.
-- Returns the memory in Kb.
getProcessMemory :: Logger -> IO (Maybe Integer)
getProcessMemory logger = do
  myPid <- getProcessID
  (exitCode, rawOutput, errors) <- readProcessWithExitCode "ps" ["-o", "rss=", "-p", show myPid] []
  case exitCode of
    ExitSuccess    -> pure $ readMaybe rawOutput
    ExitFailure ec -> do
      logger ErrorS $ "getProcessMemory exited with error code " <> T.pack (show ec) <> ": " <> T.pack errors
      pure Nothing

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
updateProcessMemory :: Logger -> P.Gauge -> Bool -> IO ()
updateProcessMemory logger g _ = do
  mbMem <- getProcessMemory logger
  case mbMem of
    Nothing -> return ()
    Just m  -> P.set (fromIntegral m) g

--------------------------------------------------------------------------------
processMemory :: Logger -> P.Gauge -> RidleyMetricHandler
processMemory logger g = do
  mkRidleyMetricHandler "ridley-process-memory" g (updateProcessMemory logger) False
