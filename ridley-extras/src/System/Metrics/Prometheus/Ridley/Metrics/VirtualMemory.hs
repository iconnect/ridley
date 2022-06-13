{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module System.Metrics.Prometheus.Ridley.Metrics.VirtualMemory where

import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Word
import qualified Data.Text as T
import           Lens.Micro
import           Shelly
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Posix.Types (ProcessID)
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read (readMaybe)

{- Calling 'vmstat' will report

[centos@atlas-eu ~]$ vmstat -s
    130701440 K total memory
     52971728 K used memory
     92670416 K active memory
     33348008 K inactive memory
       852768 K free memory
            0 K buffer memory
     76876944 K swap cache
            0 K total swap
            0 K used swap
            0 K free swap
    ...
-}

--------------------------------------------------------------------------------
getVmStats :: IO VmStatReport
getVmStats = do
  rawOutput <- shelly $ silently $ escaping False $
    mapMaybe (readMaybe @Word64 . T.unpack) . T.words . T.strip
      <$> run "vmstat" ["-s" ,"|" , "head", "-n", "10" , "|" , "awk", "-F", "\" \"" , "'{print $1}'" ]
  case rawOutput of
    [   vmstat_total_memory_kb
      , vmstat_used_memory_kb
      , vmstat_active_memory_kb
      , vmstat_inactive_memory_kb
      , vmstat_free_memory_kb
      , vmstat_buffer_memory_kb
      , vmstat_swap_cache_kb
      , vmstat_total_swap_kb
      , vmstat_used_swap_kb
      , vmstat_free_swap_kb
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
  P.set (realToFrac vmstat_total_memory_kb    ) vmstat_total_memory_kb_g
  P.set (realToFrac vmstat_used_memory_kb     ) vmstat_used_memory_kb_g
  P.set (realToFrac vmstat_active_memory_kb   ) vmstat_active_memory_kb_g
  P.set (realToFrac vmstat_inactive_memory_kb ) vmstat_inactive_memory_kb_g
  P.set (realToFrac vmstat_free_memory_kb     ) vmstat_free_memory_kb_g
  P.set (realToFrac vmstat_buffer_memory_kb   ) vmstat_buffer_memory_kb_g
  P.set (realToFrac vmstat_swap_cache_kb      ) vmstat_swap_cache_kb_g
  P.set (realToFrac vmstat_total_swap_kb      ) vmstat_total_swap_kb_g
  P.set (realToFrac vmstat_used_swap_kb       ) vmstat_used_swap_kb_g
  P.set (realToFrac vmstat_free_swap_kb       ) vmstat_free_swap_kb_g

data VmStatReport =
  VmStatReport {
    vmstat_total_memory_kb    :: !Word64
  , vmstat_used_memory_kb     :: !Word64
  , vmstat_active_memory_kb   :: !Word64
  , vmstat_inactive_memory_kb :: !Word64
  , vmstat_free_memory_kb     :: !Word64
  , vmstat_buffer_memory_kb   :: !Word64
  , vmstat_swap_cache_kb      :: !Word64
  , vmstat_total_swap_kb      :: !Word64
  , vmstat_used_swap_kb       :: !Word64
  , vmstat_free_swap_kb       :: !Word64
  } deriving (Show, Eq)

emptyVmStatReport :: VmStatReport
emptyVmStatReport = VmStatReport
  { vmstat_total_memory_kb    = 0
  , vmstat_used_memory_kb     = 0
  , vmstat_active_memory_kb   = 0
  , vmstat_inactive_memory_kb = 0
  , vmstat_free_memory_kb     = 0
  , vmstat_buffer_memory_kb   = 0
  , vmstat_swap_cache_kb      = 0
  , vmstat_total_swap_kb      = 0
  , vmstat_used_swap_kb       = 0
  , vmstat_free_swap_kb       = 0
  }


data VmStatGauges =
  VmStatGauges {
    vmstat_total_memory_kb_g    :: !P.Gauge
  , vmstat_used_memory_kb_g     :: !P.Gauge
  , vmstat_active_memory_kb_g   :: !P.Gauge
  , vmstat_inactive_memory_kb_g :: !P.Gauge
  , vmstat_free_memory_kb_g     :: !P.Gauge
  , vmstat_buffer_memory_kb_g   :: !P.Gauge
  , vmstat_swap_cache_kb_g      :: !P.Gauge
  , vmstat_total_swap_kb_g      :: !P.Gauge
  , vmstat_used_swap_kb_g       :: !P.Gauge
  , vmstat_free_swap_kb_g       :: !P.Gauge
  }

--------------------------------------------------------------------------------
-- | Returns the virtual memory total and free as sampled from 'vmstat'.
systemVirtualMemory :: MonadIO m
                    => RidleyOptions
                    -> P.RegistryT m RidleyMetricHandler
systemVirtualMemory opts = do
  let popts = opts ^. prometheusOptions
  gauges <- VmStatGauges <$> P.registerGauge "vmstat_total_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_used_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_active_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_inactive_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_free_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_buffer_memory_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_swap_cache_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_total_swap_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_used_swap_kb" (popts ^. labels)
                         <*> P.registerGauge "vmstat_free_swap_kb" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-virtual-memory-statistics" gauges updateVmStat False
