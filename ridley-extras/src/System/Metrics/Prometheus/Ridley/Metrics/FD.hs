{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module System.Metrics.Prometheus.Ridley.Metrics.FD where

import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
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
getOpenFD :: ProcessID -> IO Double
getOpenFD pid = do
  rawOutput <- shelly $ silently $ escaping False $
    T.strip <$> run "ls" ["-l", "/proc/" <> T.pack (show pid) <> "/fd", "|"
                         ,"wc", "-l"
                         ]
  return $ fromMaybe 0.0 (readMaybe . T.unpack $ rawOutput)

--------------------------------------------------------------------------------
updateOpenFD :: ProcessID -> P.Gauge -> Bool -> IO ()
updateOpenFD pid gauge _ = do
#ifdef darwin_HOST_OS
  openFd <- return 0
#else
  openFd <- getOpenFD pid
#endif
  P.set openFd gauge

--------------------------------------------------------------------------------
-- | Monitors the number of open file descriptors for a given `ProcessID`.
processOpenFD :: ProcessID -> Ridley RidleyMetricHandler
processOpenFD pid = do
  opts <- ask
  let popts = opts ^. prometheusOptions
  openFD <- lift $ P.registerGauge "process_open_fd" (popts ^. labels)
  return RidleyMetricHandler {
    metric = openFD
  , updateMetric = updateOpenFD pid
  , flush = False
  }
