{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
module System.Metrics.Prometheus.Ridley.Metrics.FD where

import qualified Data.Text as T
import           Lens.Micro
import           Shelly
import           Katip.Core
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Types (ProcessID)
import           System.Remote.Monitoring.Prometheus (labels)
import Katip.Monadic
import Data.String
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)

logAndReturnFDs :: RidleyOptions -> LogEnv -> ProcessID -> [T.Text] -> IO Double
logAndReturnFDs opts le pid descriptors = do
  let !descriptorsNums = length descriptors
  when (descriptorsNums >= opts ^. openFDWarningTreshold) $
    runRidley opts le $ do
      $(logTM) WarningS $ fromString $ "Careful, number of open file descriptors for process " <> show pid <> " exceeded warning threshold (" <> show (opts ^. openFDWarningTreshold) <> "):\n" <> T.unpack (T.unlines descriptors)
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
processOpenFD :: ProcessID
              -> Ridley RidleyMetricHandler
processOpenFD pid = do
  opts <- ask
  le   <- getLogEnv
  let popts = opts ^. prometheusOptions
  openFD <- lift $ P.registerGauge "process_open_fd" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-process-open-file-descriptors" openFD (updateOpenFD opts le pid) False
