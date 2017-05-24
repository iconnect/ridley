{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Ridley.Metrics.DiskUsage (
    getDiskStats
  , mkDiskGauge
  , diskUsageMetrics
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.TH
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           System.Process
import           Text.Read


--------------------------------------------------------------------------------
data DiskStats = DiskStats {
    _diskFilesystem :: T.Text
  , _diskUsed       :: Double
  , _diskFree       :: Double
  } deriving Show

makeLenses ''DiskStats

--------------------------------------------------------------------------------
data DiskMetric = DiskMetric {
    _dskMetricUsed :: P.Gauge
  , _dskMetricFree :: P.Gauge
  }

--------------------------------------------------------------------------------
type DiskUsageMetrics = M.Map T.Text DiskMetric

--------------------------------------------------------------------------------
getDiskStats :: IO [DiskStats]
getDiskStats = do
  let diskOnly = (\d -> "/dev" `T.isInfixOf` (d ^. diskFilesystem))
  let dropHeader = drop 1
  rawLines <- dropHeader . T.lines . T.strip . T.pack <$> readProcess "df" [] []
  return $ filter diskOnly . mapMaybe mkDiskStats $ rawLines
  where
    mkDiskStats :: T.Text -> Maybe DiskStats
    mkDiskStats rawLine = case T.words rawLine of
#ifdef darwin_HOST_OS
     [fs,_, used,free,_,_,_,_,_] -> DiskStats <$> pure fs
                                              <*> readMaybe (T.unpack used)
                                              <*> readMaybe (T.unpack free)
#else
     -- On Linux, `df` shows less things by default, example
     -- Filesystem     1K-blocks     Used Available Use% Mounted on
     -- /dev/xvda1      52416860 27408532  25008328  53% /
     [fs,_, used,free,_,_] -> DiskStats <$> pure fs
                                        <*> readMaybe (T.unpack used)
                                        <*> readMaybe (T.unpack free)
#endif
     _ -> Nothing

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
updateDiskUsageMetric :: DiskMetric -> DiskStats -> Bool -> IO ()
updateDiskUsageMetric DiskMetric{..} d _ = do
  P.set (d ^. diskUsed) _dskMetricUsed
  P.set (d ^. diskFree) _dskMetricFree

--------------------------------------------------------------------------------
updateDiskUsageMetrics :: DiskUsageMetrics -> Bool -> IO ()
updateDiskUsageMetrics dmetrics flush = do
  diskStats <- getDiskStats
  forM_ diskStats $ \d -> do
    let key = d ^. diskFilesystem
    case M.lookup key dmetrics of
      Nothing -> return ()
      Just m  -> updateDiskUsageMetric m d flush

--------------------------------------------------------------------------------
diskUsageMetrics :: DiskUsageMetrics -> RidleyMetricHandler
diskUsageMetrics g = RidleyMetricHandler {
    metric = g
  , updateMetric = updateDiskUsageMetrics
  , flush = False
  }

--------------------------------------------------------------------------------
mkDiskGauge :: MonadIO m => P.Labels -> DiskUsageMetrics -> DiskStats -> P.RegistryT m DiskUsageMetrics
mkDiskGauge currentLabels dmap d = do
  let fs = d ^. diskFilesystem
  let finalLabels = P.addLabel "filesystem" fs currentLabels
  metric <- DiskMetric <$> P.registerGauge "disk_used_bytes_blocks" finalLabels
                       <*> P.registerGauge "disk_free_bytes_blocks" finalLabels
  liftIO $ updateDiskUsageMetric metric d False
  return $! M.insert fs metric $! dmap
