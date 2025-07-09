{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Ridley.Metrics.DiskUsage (
  -- * Types
    DiskStats(..)
  , Logger
  , Severity(..)

  -- * Handlers
  , newDiskUsageMetrics

  -- * Utility functions
  , getDiskStats
  , diskUsedPercent
  , diskFreePercent
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Maybe
import           Katip
import           Lens.Micro
import           Lens.Micro.TH
import           System.Exit
import           System.Metrics.Prometheus.Ridley.Types
import           System.Metrics.Prometheus.Ridley.Types.Internal
import           System.Process
import           System.Remote.Monitoring.Prometheus (labels)
import           Text.Read hiding (lift)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P


--------------------------------------------------------------------------------
data DiskStats = DiskStats {
    _diskFilesystem  :: !T.Text
  , _diskUsedBytes   :: !Double
  , _diskUsedPercent :: !Int
  , _diskFreeBytes   :: !Double
  , _diskFreePercent :: !Int
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
getDiskStats :: Logger -> IO [DiskStats]
getDiskStats logger = do
  let diskOnly = (\d -> "/dev" `T.isInfixOf` (d ^. diskFilesystem))
  let dropHeader = drop 1 . T.lines . T.strip . T.pack
  (exitCode, rawLines, errors) <- readProcessWithExitCode "df" [] []
  case exitCode of
    ExitSuccess    -> return $ filter diskOnly . mapMaybe mkDiskStats $ dropHeader rawLines
    ExitFailure ec -> do
      logger ErrorS $ "getDiskStats exited with error code " <> T.pack (show ec) <> ": " <> T.pack errors
      pure mempty
  where
    mkDiskStats :: T.Text -> Maybe DiskStats
    mkDiskStats rawLine = do
     (fs, used, free) <- case T.words rawLine of
#ifdef darwin_HOST_OS
       [fs,_, used,free,_,_,_,_,_] -> do
         usedBytes <- readMaybe (T.unpack used)
         freeBytes <- readMaybe (T.unpack free)
         pure (fs, usedBytes, freeBytes)
#else
       -- On Linux, `df` shows less things by default, example
       -- Filesystem     1K-blocks     Used Available Use% Mounted on
       -- /dev/xvda1      52416860 27408532  25008328  53% /
       [fs,_, used,free,_,_] -> do
         usedBytes <- readMaybe (T.unpack used)
         freeBytes <- readMaybe (T.unpack free)
         pure (fs, usedBytes, freeBytes)
#endif
       _                     -> Nothing

     let usedPercent = computeUsedPercent used free
     pure $ DiskStats {
                _diskFilesystem  = fs
              , _diskUsedBytes   = used
              , _diskUsedPercent = usedPercent
              , _diskFreeBytes   = free
              , _diskFreePercent = 100 - usedPercent
              }

computeUsedPercent :: Double -> Double -> Int
computeUsedPercent usedBytes freeBytes =
  let totalBytes = usedBytes + freeBytes
  in round $ (100.0 * usedBytes) / totalBytes

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
updateDiskUsageMetric :: DiskMetric -> DiskStats -> Bool -> IO ()
updateDiskUsageMetric DiskMetric{..} d _ = do
  P.set (d ^. diskUsedBytes) _dskMetricUsed
  P.set (d ^. diskFreeBytes) _dskMetricFree

--------------------------------------------------------------------------------
updateDiskUsageMetrics :: Logger -> DiskUsageMetrics -> Bool -> IO ()
updateDiskUsageMetrics logger dmetrics flush = do
  diskStats <- getDiskStats logger
  forM_ diskStats $ \d -> do
    let key = d ^. diskFilesystem
    case M.lookup key dmetrics of
      Nothing -> return ()
      Just m  -> updateDiskUsageMetric m d flush

--------------------------------------------------------------------------------
mkDiskGauge :: MonadIO m => P.Labels -> DiskUsageMetrics -> DiskStats -> P.RegistryT m DiskUsageMetrics
mkDiskGauge currentLabels dmap d = do
  let fs = d ^. diskFilesystem
  let finalLabels = P.addLabel "filesystem" fs currentLabels
  metric <- DiskMetric <$> P.registerGauge "disk_used_bytes_blocks" finalLabels
                       <*> P.registerGauge "disk_free_bytes_blocks" finalLabels
  liftIO $ updateDiskUsageMetric metric d False
  return $! M.insert fs metric $! dmap

-- | Creates a new 'RidleyMetricHandler' to monitor disk usage.
newDiskUsageMetrics :: Ridley RidleyMetricHandler
newDiskUsageMetrics = do
  logger <- ioLogger
  opts <- getRidleyOptions
  diskStats <- liftIO (getDiskStats logger)
  metrics   <- lift $ foldM (mkDiskGauge (opts ^. prometheusOptions . labels)) M.empty diskStats
  pure $ mkRidleyMetricHandler "ridley-disk-usage" metrics (updateDiskUsageMetrics logger) False
