{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module System.Metrics.Prometheus.Ridley.Metrics.VirtualMemory where

import           Control.Monad.IO.Class
import           Control.Monad.Reader (ask, lift)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Word
import           Lens.Micro
import           Shelly
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Types (ProcessID)
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read (readMaybe)
import qualified Data.Text as T
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P

{- Calling 'vmstat' will report

[centos@atlas-eu ~]$ vmstat -S M -s
    130701440 M total memory
     52971728 M used memory
     92670416 M active memory
     33348008 M inactive memory
       852768 M free memory
            0 M buffer memory
     76876944 M swap cache
            0 M total swap
            0 M used swap
            0 M free swap
    ...
-}

--------------------------------------------------------------------------------
getVmStats :: IO VmStatReport
getVmStats = do
  rawOutput <- shelly $ silently $ escaping False $
    mapMaybe (readMaybe @Word64 . T.unpack) . T.words . T.strip
      <$> run "vmstat" ["-S", "M", "-s" ,"|" , "head", "-n", "10" , "|" , "awk", "-F", "\" \"" , "'{print $1}'" ]
  case rawOutput of
    [   vmstat_total_memory_mb
      , vmstat_used_memory_mb
      , vmstat_active_memory_mb
      , vmstat_inactive_memory_mb
      , vmstat_free_memory_mb
      , vmstat_buffer_memory_mb
      , vmstat_swap_cache_mb
      , vmstat_total_swap_mb
      , vmstat_used_swap_mb
      , vmstat_free_swap_mb
      ] -> pure $ VmStatReport{..}
    _ -> pure emptyVmStatReport

--------------------------------------------------------------------------------
updateVmStat :: VmStatGauges -> Bool -> IO ()
updateVmStat VmStatGauges{..} _ = do
#ifdef darwin_HOST_OS
  let VmStatReport{..} = emptyVmStatReport-- "vmstat" is not available on Darwin.
#else
  VmStatReport{..} <- getVmStats
#endif
  P.set (realToFrac vmstat_total_memory_mb    ) vmstat_total_memory_mb_g
  P.set (realToFrac vmstat_used_memory_mb     ) vmstat_used_memory_mb_g
  P.set (realToFrac vmstat_active_memory_mb   ) vmstat_active_memory_mb_g
  P.set (realToFrac vmstat_inactive_memory_mb ) vmstat_inactive_memory_mb_g
  P.set (realToFrac vmstat_free_memory_mb     ) vmstat_free_memory_mb_g
  P.set (realToFrac vmstat_buffer_memory_mb   ) vmstat_buffer_memory_mb_g
  P.set (realToFrac vmstat_swap_cache_mb      ) vmstat_swap_cache_mb_g
  P.set (realToFrac vmstat_total_swap_mb      ) vmstat_total_swap_mb_g
  P.set (realToFrac vmstat_used_swap_mb       ) vmstat_used_swap_mb_g
  P.set (realToFrac vmstat_free_swap_mb       ) vmstat_free_swap_mb_g

data VmStatReport =
  VmStatReport {
    vmstat_total_memory_mb    :: !Word64
  , vmstat_used_memory_mb     :: !Word64
  , vmstat_active_memory_mb   :: !Word64
  , vmstat_inactive_memory_mb :: !Word64
  , vmstat_free_memory_mb     :: !Word64
  , vmstat_buffer_memory_mb   :: !Word64
  , vmstat_swap_cache_mb      :: !Word64
  , vmstat_total_swap_mb      :: !Word64
  , vmstat_used_swap_mb       :: !Word64
  , vmstat_free_swap_mb       :: !Word64
  } deriving (Show, Eq)

emptyVmStatReport :: VmStatReport
emptyVmStatReport = VmStatReport
  { vmstat_total_memory_mb    = 0
  , vmstat_used_memory_mb     = 0
  , vmstat_active_memory_mb   = 0
  , vmstat_inactive_memory_mb = 0
  , vmstat_free_memory_mb     = 0
  , vmstat_buffer_memory_mb   = 0
  , vmstat_swap_cache_mb      = 0
  , vmstat_total_swap_mb      = 0
  , vmstat_used_swap_mb       = 0
  , vmstat_free_swap_mb       = 0
  }


data VmStatGauges =
  VmStatGauges {
    vmstat_total_memory_mb_g    :: !P.Gauge
  , vmstat_used_memory_mb_g     :: !P.Gauge
  , vmstat_active_memory_mb_g   :: !P.Gauge
  , vmstat_inactive_memory_mb_g :: !P.Gauge
  , vmstat_free_memory_mb_g     :: !P.Gauge
  , vmstat_buffer_memory_mb_g   :: !P.Gauge
  , vmstat_swap_cache_mb_g      :: !P.Gauge
  , vmstat_total_swap_mb_g      :: !P.Gauge
  , vmstat_used_swap_mb_g       :: !P.Gauge
  , vmstat_free_swap_mb_g       :: !P.Gauge
  }

--------------------------------------------------------------------------------
-- | Returns the virtual memory total and free as sampled from 'vmstat'.
systemVirtualMemory :: Ridley RidleyMetricHandler
systemVirtualMemory = do
  opts <- ask
  let popts = opts ^. prometheusOptions
  gauges <- lift $ VmStatGauges <$> P.registerGauge "vmstat_total_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_used_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_active_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_inactive_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_free_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_buffer_memory_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_swap_cache_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_total_swap_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_used_swap_mb" (popts ^. labels)
                                <*> P.registerGauge "vmstat_free_swap_mb" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-virtual-memory-statistics" gauges updateVmStat False
