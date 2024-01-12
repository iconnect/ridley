{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
module System.Metrics.Prometheus.Ridley.Metrics.FD where

import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text as T
import           Lens.Micro
import           Shelly
import           Katip.Core
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Types (ProcessID)
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read (readMaybe)
import Katip.Monadic
import Data.String

logAndReturnFDs :: RidleyOptions -> LogEnv -> ProcessID -> [T.Text] -> IO Double
logAndReturnFDs opts le pid descriptors = do
  let !descriptorsNums = length descriptors
  when (descriptorsNums >= opts ^. openFDWarningTreshold) $
    runRidley opts le $ do
      $(logTM) WarningS $ fromString $ "Careful, there are a suspiciously high number of open file descriptors for process " <> show pid <> ": " <> T.unpack (T.unlines descriptors)
  return $ (fromIntegral $ descriptorsNums)

--------------------------------------------------------------------------------
getOpenFD_unix :: RidleyOptions -> LogEnv -> ProcessID -> IO Double
getOpenFD_unix opts le pid = do
  descriptors <- shelly $ silently $ escaping False $
    T.lines . T.strip <$> run "ls" ["-l", "/proc/" <> T.pack (show pid) <> "/fd", "|"
                         ,"grep", "^l"
                         ]
  logAndReturnFDs opts le pid descriptors

--------------------------------------------------------------------------------
getOpenFD_darwin :: RidleyOptions -> LogEnv -> ProcessID -> IO Double
getOpenFD_darwin opts le pid = do
  descriptors <- shelly $ silently $ escaping False $
    T.lines . T.strip <$> run "lsof" ["-p", T.pack (show pid), "|"
                           ,"grep", "REG", "|", "awk", "'{print $9}'"
                           ]
  logAndReturnFDs opts le pid descriptors

--------------------------------------------------------------------------------
updateOpenFD :: RidleyOptions -> LogEnv -> ProcessID -> P.Gauge -> Bool -> IO ()
updateOpenFD opts le pid gauge _ = do
#ifdef darwin_HOST_OS
  openFd <- getOpenFD_darwin opts le pid
#else
  openFd <- getOpenFD_unix opts le pid
#endif
  P.set openFd gauge

--------------------------------------------------------------------------------
-- | Monitors the number of open file descriptors for a given `ProcessID`.
processOpenFD :: MonadIO m
              => ProcessID
              -> RidleyOptions
              -> LogEnv
              -> P.RegistryT m RidleyMetricHandler
processOpenFD pid opts le = do
  let popts = opts ^. prometheusOptions
  openFD <- P.registerGauge "process_open_fd" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-process-open-file-descriptors" openFD (updateOpenFD opts le pid) False
