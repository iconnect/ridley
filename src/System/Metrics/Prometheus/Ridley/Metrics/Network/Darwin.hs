{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Ridley.Metrics.Network.Darwin
  ( networkMetrics
  , getNetworkMetrics
  , mkInterfaceGauge
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Metrics.Network.Types
import           System.Metrics.Prometheus.Ridley.Types
import           Text.RawString.QQ (r)

C.context (C.baseCtx <> C.vecCtx <> ifDataCtx)
C.include "<sys/types.h>"
C.include "<sys/socket.h>"
C.include "<ifaddrs.h>"
C.include "<regex.h>"
C.include "<stddef.h>"
C.include "helpers.h"
C.include "<stdlib.h>"
C.include "<string.h>"
C.include "<net/if.h>"


C.verbatim [r|
void set_ridley_ifi_data(struct if_data* netData, ridley_if_data_t* devStats) {

  //devStats["receive_packets"] = convertFreeBSDCPUTime(uint64(netData.ifi_ipackets));
  devStats->ridley_ifi_ipackets = (long)(netData->ifi_ipackets);

  //devStats["transmit_packets"] = convertFreeBSDCPUTime(uint64(netData.ifi_opackets));
  devStats->ridley_ifi_opackets = (long)(netData->ifi_opackets);

  //devStats["receive_errs"] = convertFreeBSDCPUTime(uint64(netData.ifi_ierrors));
  devStats->ridley_ifi_ierrors = (long)(netData->ifi_ierrors);

  //devStats["transmit_errs"] = convertFreeBSDCPUTime(uint64(netData.ifi_oerrors));
  devStats->ridley_ifi_oerrors = (long)(netData->ifi_oerrors);

  //devStats["receive_bytes"] = convertFreeBSDCPUTime(uint64(netData.ifi_ibytes));
  devStats->ridley_ifi_ibytes  = (long)(netData->ifi_ibytes);

  //devStats["transmit_bytes"] = convertFreeBSDCPUTime(uint64(netData.ifi_obytes));
  devStats->ridley_ifi_obytes  = (long)(netData->ifi_obytes);

  //devStats["receive_multicast"] = convertFreeBSDCPUTime(uint64(netData.ifi_imcasts));
  devStats->ridley_ifi_imcasts  = (long)(netData->ifi_imcasts);
  //devStats["transmit_multicast"] = convertFreeBSDCPUTime(uint64(netData.ifi_omcasts));
  devStats->ridley_ifi_omcasts  = (long)(netData->ifi_omcasts);
  //devStats["receive_drop"] = convertFreeBSDCPUTime(uint64(netData.ifi_iqdrops));
  devStats->ridley_ifi_iqdrops = (long)(netData->ifi_iqdrops);

  //devStats["transmit_drop"] = convertFreeBSDCPUTime(uint64(netData.ifi_oqdrops));
  // Not present in this version of if_data
}
|]

--------------------------------------------------------------------------------
getNetworkMetrics' :: IO (Ptr IfData, Ptr C.CInt)
getNetworkMetrics' = do
  (totalInterfaces :: Ptr C.CInt) <- malloc
  res <- [C.block| ridley_if_data_t* {
        struct ifaddrs *ifap, *ifa;
        struct if_data *netData;
        ridley_if_data_t *netDev = malloc(30 * sizeof(ridley_if_data_t));
        int interfaceIdx = 0;

        // Compile a regex to ignore certain devices
        regex_t regex;
        int ignoreDevice;

        ignoreDevice = regcomp(&regex, "^(ram|loop|fd|(h|s|v|xv)d[a-z]|nvme\\d+n\\d+p)\\d+$$", 0);

        if (ignoreDevice) {
          netDev[0].ridley_ifi_error = 1;
          return netDev;
        }

        if (getifaddrs(&ifap) == -1) {
          netDev[0].ridley_ifi_error = 1;
          return netDev;
        }

        // Iterate over all the network interfaces found
        for (ifa = ifap; ifa; ifa = ifa->ifa_next) {
          if (ifa->ifa_addr->sa_family == AF_LINK) {
             char* currentDevice = malloc((strlen(ifa->ifa_name) + 1) * sizeof(char));
             strcpy(currentDevice, ifa->ifa_name);
             ignoreDevice = regexec(&regex, currentDevice, 0, NULL, 0);
             if (!ignoreDevice) {
                 regfree(&regex);
                 free(currentDevice);
                 continue;
             } else {
                 regfree(&regex);
             }

             ridley_if_data_t *devStats = &netDev[interfaceIdx];
             netData = ifa->ifa_data;
             devStats->ridley_ifi_name = currentDevice;
             set_ridley_ifi_data(netData, devStats);
             devStats->ridley_ifi_error = 0;
             interfaceIdx++;

          }
        }
        freeifaddrs(ifap);
        *$(int* totalInterfaces) = interfaceIdx;
        return netDev;
       }
       |]
  return (res, totalInterfaces)

--------------------------------------------------------------------------------
getNetworkMetrics :: IO ([IfData], IO ())
getNetworkMetrics = do
  (raw, total)  <- getNetworkMetrics'
  (CInt interfacesNum) <- peek total
  m   <- peekArray (fromIntegral interfacesNum) raw
  let dtor = freeRidleyIFData raw (CInt interfacesNum)
  free total
  return (m, dtor)

--------------------------------------------------------------------------------
-- | As this is a gauge, it makes no sense flushing it.
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
updateNetworkMetrics nmetrics flush = do
  (ifaces, dtor) <- getNetworkMetrics
  forM_ ifaces $ \d@IfData{..} -> do
    key <- T.pack <$> peekCAString ifi_name
    case M.lookup key nmetrics of
      Nothing -> return ()
      Just m  -> updateNetworkMetric m d flush
  dtor


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
  iname <- T.pack <$> liftIO (peekCAString ifi_name)
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
