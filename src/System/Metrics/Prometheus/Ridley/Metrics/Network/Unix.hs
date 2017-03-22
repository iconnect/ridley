{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Metrics.Prometheus.Ridley.Metrics.Network.Unix
  ( networkMetrics
  , getNetworkMetrics
  , mkInterfaceGauge
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (FilePath)
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Metrics.Network.Types
import           System.Metrics.Prometheus.Ridley.Types

--------------------------------------------------------------------------------
-- | Parse /proc/net/dev to get the relevant stats.
getNetworkMetrics :: IO [IfData]
getNetworkMetrics = do
  interfaces <- drop 2 . T.lines . T.strip <$> T.readFile "/proc/net/dev"
  return $! mapMaybe mkInterface interfaces
  where
    mkInterface :: T.Text -> Maybe IfData
    mkInterface rawLine = case T.words . T.strip $ rawLine of
      [iface, ibytes, ipackets, ierrs, idrop, _, _, _, imulticast, obytes, opackets, oerrs, _, _, _, _, _] ->
        Just $ IfData {
            ifi_ipackets = read $ T.unpack ipackets
          , ifi_opackets = read $ T.unpack opackets
          , ifi_ierrors  = read $ T.unpack ierrs
          , ifi_oerrors  = read $ T.unpack oerrs
          , ifi_ibytes   = read $ T.unpack ibytes
          , ifi_obytes   = read $ T.unpack obytes
          , ifi_imcasts  = read $ T.unpack imulticast
          , ifi_omcasts  = 0
          , ifi_iqdrops  = read $ T.unpack idrop
          , ifi_name     = T.unpack $ T.init iface
          , ifi_error    = 0
          }
      _  -> Nothing

--------------------------------------------------------------------------------
updateNetworkMetric :: NetworkMetric -> IfData -> Bool -> IO ()
updateNetworkMetric NetworkMetric{..} IfData{..} _ = do
  P.set (fromIntegral ifi_ipackets) receive_packets
  P.set (fromIntegral ifi_opackets) transmit_packets
  P.set (fromIntegral ifi_ierrors) receive_errs
  P.set (fromIntegral ifi_oerrors) transmit_errs
  P.set (fromIntegral ifi_ibytes) receive_bytes
  P.set (fromIntegral ifi_obytes) transmit_bytes
  P.set (fromIntegral ifi_imcasts) receive_multicast
  P.set (fromIntegral ifi_omcasts) transmit_multicast
  P.set (fromIntegral ifi_iqdrops) receive_drop

--------------------------------------------------------------------------------
updateNetworkMetrics :: NetworkMetrics -> Bool -> IO ()
updateNetworkMetrics nmetrics mustFlush = do
  ifaces <- getNetworkMetrics
  forM_ ifaces $ \d@IfData{..} -> do
    let key = T.pack ifi_name
    case M.lookup key nmetrics of
      Nothing -> return ()
      Just m  -> updateNetworkMetric m d mustFlush

--------------------------------------------------------------------------------
networkMetrics :: NetworkMetrics -> RidleyMetricHandler
networkMetrics g = RidleyMetricHandler {
    metric = g
  , updateMetric = updateNetworkMetrics
  , flush = False
  }

--------------------------------------------------------------------------------
mkInterfaceGauge :: MonadIO m => P.Labels -> NetworkMetrics -> IfData -> P.RegistryT m NetworkMetrics
mkInterfaceGauge currentLabels imap d@IfData{..} = do
  let iname = T.pack ifi_name
  let finalLabels = P.addLabel "interface" iname currentLabels
  metric <- NetworkMetric <$> P.registerGauge "network_receive_packets"    finalLabels
                          <*> P.registerGauge "network_transmit_packets"   finalLabels
                          <*> P.registerGauge "network_receive_errs"       finalLabels
                          <*> P.registerGauge "network_transmit_errs"      finalLabels
                          <*> P.registerGauge "network_receive_bytes"      finalLabels
                          <*> P.registerGauge "network_transmit_bytes"     finalLabels
                          <*> P.registerGauge "network_receive_multicast"  finalLabels
                          <*> P.registerGauge "network_transmit_multicast" finalLabels
                          <*> P.registerGauge "network_receive_drop"       finalLabels
  liftIO $ updateNetworkMetric metric d False
  return $! M.insert iname metric $! imap
