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
  , newDiskUsageMetricsWith

  -- * Utility functions
  , getDiskStats
  , diskUsedPercent
  , diskFreePercent
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
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
import qualified Control.Exception.Safe as Ex
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Metrics.Prometheus.Concurrent.Registry as PC
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.MetricId as P


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
  -- Run df directly, without an intermediate shell: /bin/sh is dash on
  -- Debian/Ubuntu, which does not support `set -o pipefail` and made this
  -- fail unconditionally. Deduplication by filesystem (for btrfs/zfs
  -- systems, where one device appears under several mount points) is done
  -- in dedupByFilesystem below.
  (exitCode, rawLines, errors) <- readProcessWithExitCode "df" [] []
  case exitCode of
    ExitSuccess    -> return $ filter diskOnly . dedupByFilesystem . mapMaybe mkDiskStats $ dropHeader rawLines
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

-- | Keep only the first 'DiskStats' for each filesystem: on btrfs/zfs the
-- same device is reported by df once per subvolume/mount point.
dedupByFilesystem :: [DiskStats] -> [DiskStats]
dedupByFilesystem = go Set.empty
  where
    go _ [] = []
    go seen (d:ds)
      | fs `Set.member` seen = go seen ds
      | otherwise            = d : go (Set.insert fs seen) ds
      where fs = d ^. diskFilesystem

computeUsedPercent :: Double -> Double -> Int
computeUsedPercent usedBytes freeBytes =
  let totalBytes = usedBytes + freeBytes
  in round $ (100.0 * usedBytes) / totalBytes

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
updateDiskUsageMetric :: DiskMetric -> DiskStats -> IO ()
updateDiskUsageMetric DiskMetric{..} d = do
  P.set (d ^. diskUsedBytes) _dskMetricUsed
  P.set (d ^. diskFreeBytes) _dskMetricFree

--------------------------------------------------------------------------------
-- | The state threaded through the update loop: the gauges registered so far,
-- keyed by filesystem name. Gauges for filesystems not seen before are
-- registered on the fly, which makes this handler self-healing: a transient
-- failure of the stats runner (at boot or at any later point) merely delays
-- the registration until the next successful run (see iconnect/hermes#2596,
-- where a transient @posix_spawnp@ failure at boot silenced disk metrics
-- until the server was restarted).
data DiskUsageState = DiskUsageState {
    _dskRegistry :: PC.Registry
  , _dskLabels   :: P.Labels
  , _dskMetrics  :: IORef DiskUsageMetrics
  }

--------------------------------------------------------------------------------
updateDiskUsageMetrics :: Logger -> IO [DiskStats] -> DiskUsageState -> Bool -> IO ()
updateDiskUsageMetrics logger runStats DiskUsageState{..} _ = do
  diskStats <- runStats
  when (null diskStats) $
    logger WarningS "getDiskStats returned no filesystems: no disk usage metric will be reported."
  forM_ diskStats $ \d -> do
    let key = d ^. diskFilesystem
    dmetrics <- readIORef _dskMetrics
    m <- case M.lookup key dmetrics of
      Just m  -> pure m
      Nothing -> do
        m <- mkDiskGauge _dskRegistry _dskLabels d
        atomicModifyIORef' _dskMetrics (\dm -> (M.insert key m dm, ()))
        pure m
    updateDiskUsageMetric m d

--------------------------------------------------------------------------------
mkDiskGauge :: PC.Registry -> P.Labels -> DiskStats -> IO DiskMetric
mkDiskGauge registry currentLabels d = do
  let fs = d ^. diskFilesystem
  let finalLabels = P.addLabel "filesystem" fs currentLabels
  DiskMetric <$> PC.registerGauge "disk_used_bytes_blocks" finalLabels registry
             <*> PC.registerGauge "disk_free_bytes_blocks" finalLabels registry

-- | Creates a new 'RidleyMetricHandler' to monitor disk usage.
newDiskUsageMetrics :: Ridley RidleyMetricHandler
newDiskUsageMetrics = do
  logger <- ioLogger
  newDiskUsageMetricsWith (getDiskStats logger)

-- | Like 'newDiskUsageMetrics', but with a user-supplied action to produce
-- the 'DiskStats'. The disk gauges are registered lazily, from the update
-- loop: a failure of the supplied action never disables the metric
-- permanently, it only delays it until the action recovers.
newDiskUsageMetricsWith :: IO [DiskStats] -> Ridley RidleyMetricHandler
newDiskUsageMetricsWith runStats = do
  logger   <- ioLogger
  opts     <- getRidleyOptions
  registry <- getRidleyRegistry
  st <- DiskUsageState registry (opts ^. prometheusOptions . labels) <$> liftIO (newIORef M.empty)
  let handler = mkRidleyMetricHandler "ridley-disk-usage" st (updateDiskUsageMetrics logger runStats) False
  -- Populate the gauges eagerly so they are available right after boot, but
  -- only as a best effort: on failure the update loop will try again.
  liftIO $ runHandler handler `Ex.catchAny` \ex ->
    logger ErrorS $ "Initial disk usage collection failed (the update loop will retry): "
                 <> T.pack (Ex.displayException ex)
  pure handler
