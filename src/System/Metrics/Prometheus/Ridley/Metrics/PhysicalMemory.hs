{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module System.Metrics.Prometheus.Ridley.Metrics.PhysicalMemory where

import           Data.Maybe (mapMaybe)
import           Data.Word
import qualified Data.Text as T
import           Lens.Micro
import           Shelly
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read (readMaybe)
import Control.Monad.Reader

{- Calling 'free' will report
[service-runner@hermes-devel ~]$ free -k
              total        used        free      shared  buff/cache   available
Mem:            962         377         251          36         333         377
Swap:          4095           0        4095
-}

--------------------------------------------------------------------------------
getFreeStats :: IO FreeReport
getFreeStats = do
  rawOutput <- shelly $ silently $ escaping False $
    mapMaybe (readMaybe @Word64 . T.unpack) . T.words . T.strip <$> run "free" ["-m" ,"|" , "tail", "-n", "-2" , "|", "awk", "-F", "\" \"", "'{printf \"%s %s %s %s %s %s %s\", $2, $3, $4, $5, $6, $7, $8}'"]
  case rawOutput of
    [   free_mem_total_mb
      , free_mem_used_mb
      , free_mem_free_mb
      , free_mem_shared_mb
      , free_mem_buff_cache_mb
      , free_mem_available_mb
      , free_swap_total_mb
      , free_swap_used_mb
      , free_swap_free_mb
      ] -> pure $ FreeReport{..}
    _ -> pure emptyFreeReport

--------------------------------------------------------------------------------
updateFreeStats :: FreeGauges -> Bool -> IO ()
updateFreeStats FreeGauges{..} _ = do
#ifdef darwin_HOST_OS
  let FreeReport{..} = emptyFreeReport-- "free" is not available on Darwin.
#else
  FreeReport{..} <- getFreeStats
#endif
  P.set (realToFrac free_mem_total_mb      ) free_mem_total_mb_g
  P.set (realToFrac free_mem_used_mb       ) free_mem_used_mb_g
  P.set (realToFrac free_mem_free_mb       ) free_mem_free_mb_g
  P.set (realToFrac free_mem_shared_mb     ) free_mem_shared_mb_g
  P.set (realToFrac free_mem_buff_cache_mb ) free_mem_buff_cache_mb_g
  P.set (realToFrac free_mem_available_mb  ) free_mem_available_mb_g
  P.set (realToFrac free_swap_total_mb     ) free_swap_total_mb_g
  P.set (realToFrac free_swap_used_mb      ) free_swap_used_mb_g
  P.set (realToFrac free_swap_free_mb      ) free_swap_free_mb_g

data FreeReport =
  FreeReport {
    free_mem_total_mb      :: !Word64
  , free_mem_used_mb       :: !Word64
  , free_mem_free_mb       :: !Word64
  , free_mem_shared_mb     :: !Word64
  , free_mem_buff_cache_mb :: !Word64
  , free_mem_available_mb  :: !Word64
  , free_swap_total_mb     :: !Word64
  , free_swap_used_mb      :: !Word64
  , free_swap_free_mb      :: !Word64
  } deriving (Show, Eq)

emptyFreeReport :: FreeReport
emptyFreeReport = FreeReport
  { free_mem_total_mb      = 0
  , free_mem_used_mb       = 0
  , free_mem_free_mb       = 0
  , free_mem_shared_mb     = 0
  , free_mem_buff_cache_mb = 0
  , free_mem_available_mb  = 0
  , free_swap_total_mb     = 0
  , free_swap_used_mb      = 0
  , free_swap_free_mb      = 0
  }


data FreeGauges =
  FreeGauges {
    free_mem_total_mb_g      :: !P.Gauge
  , free_mem_used_mb_g       :: !P.Gauge
  , free_mem_free_mb_g       :: !P.Gauge
  , free_mem_shared_mb_g     :: !P.Gauge
  , free_mem_buff_cache_mb_g :: !P.Gauge
  , free_mem_available_mb_g  :: !P.Gauge
  , free_swap_total_mb_g     :: !P.Gauge
  , free_swap_used_mb_g      :: !P.Gauge
  , free_swap_free_mb_g      :: !P.Gauge
  }

--------------------------------------------------------------------------------
-- | Returns the physical memory total and free as sampled from 'free'.
systemPhysicalMemory :: Ridley RidleyMetricHandler
systemPhysicalMemory = do
  opts <- ask
  let popts = opts ^. prometheusOptions
  gauges <- lift $ FreeGauges <$> P.registerGauge "free_mem_total_mb" (popts ^. labels)
                              <*> P.registerGauge "free_mem_used_mb" (popts ^. labels)
                              <*> P.registerGauge "free_mem_free_mb" (popts ^. labels)
                              <*> P.registerGauge "free_mem_shared_mb" (popts ^. labels)
                              <*> P.registerGauge "free_mem_buff_cache_mb" (popts ^. labels)
                              <*> P.registerGauge "free_mem_available_mb" (popts ^. labels)
                              <*> P.registerGauge "free_swap_total_mb" (popts ^. labels)
                              <*> P.registerGauge "free_swap_used_mb" (popts ^. labels)
                              <*> P.registerGauge "free_swap_free_mb" (popts ^. labels)
  return $ mkRidleyMetricHandler "ridley-physical-memory-statistics" gauges updateFreeStats False
