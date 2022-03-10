{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Ridley.Metrics.FD where

import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text as T
import           Lens.Micro
import           Shelly
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Types (ProcessID)
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read (readMaybe)

--------------------------------------------------------------------------------
getOpenFD_unix :: ProcessID -> IO Double
getOpenFD_unix pid = do
  rawOutput <- shelly $ silently $ escaping False $
    T.strip <$> run "ls" ["-l", "/proc/" <> T.pack (show pid) <> "/fd", "|"
                         ,"wc", "-l"
                         ]
  return $ fromMaybe 0.0 (readMaybe . T.unpack $ rawOutput)

--------------------------------------------------------------------------------
getOpenFD_darwin :: ProcessID -> IO Double
getOpenFD_darwin pid = do
  rawOutput <- shelly $ silently $ escaping False $
    T.strip <$> run "lsof" ["-p", T.pack (show pid), "|"
                           ,"wc", "-l"
                           ]
  return $ fromMaybe 0.0 (readMaybe . T.unpack $ rawOutput)

--------------------------------------------------------------------------------
updateOpenFD :: ProcessID -> P.Gauge -> Bool -> IO ()
updateOpenFD pid gauge _ = do
#ifdef darwin_HOST_OS
  openFd <- getOpenFD_darwin pid
#else
  openFd <- getOpenFD_unix pid
#endif
  P.set openFd gauge

--------------------------------------------------------------------------------
-- | Monitors the number of open file descriptors for a given `ProcessID`.
processOpenFD :: MonadIO m
              => ProcessID
              -> RidleyOptions
              -> P.RegistryT m RidleyMetricHandler
processOpenFD pid opts = do
  let popts = opts ^. prometheusOptions
  openFD <- P.registerGauge "process_open_fd" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-process-open-file-descriptors" openFD (updateOpenFD pid) False
