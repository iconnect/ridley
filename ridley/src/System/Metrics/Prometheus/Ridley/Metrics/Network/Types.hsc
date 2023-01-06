{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module System.Metrics.Prometheus.Ridley.Metrics.Network.Types where

import qualified Data.Map as Map
import           Data.Map.Strict as M
import qualified Data.Text as T
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable (Storable(..))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import qualified System.Metrics.Prometheus.Metric.Gauge as P

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include "helpers.h"

data IfData = IfData {
#ifdef darwin_HOST_OS
    ifi_ipackets :: {-# UNPACK #-} !CUInt
  , ifi_opackets :: {-# UNPACK #-} !CUInt
  , ifi_ierrors  :: {-# UNPACK #-} !CUInt
  , ifi_oerrors  :: {-# UNPACK #-} !CUInt
  , ifi_ibytes   :: {-# UNPACK #-} !CUInt
  , ifi_obytes   :: {-# UNPACK #-} !CUInt
  , ifi_imcasts  :: {-# UNPACK #-} !CUInt
  , ifi_omcasts  :: {-# UNPACK #-} !CUInt
  , ifi_iqdrops  :: {-# UNPACK #-} !CUInt
  , ifi_name     :: {-# UNPACK #-} !CString
  , ifi_error    :: {-# UNPACK #-} !CInt
# else
    ifi_ipackets :: {-# UNPACK #-} !Int
  , ifi_opackets :: {-# UNPACK #-} !Int
  , ifi_ierrors  :: {-# UNPACK #-} !Int
  , ifi_oerrors  :: {-# UNPACK #-} !Int
  , ifi_ibytes   :: {-# UNPACK #-} !Int
  , ifi_obytes   :: {-# UNPACK #-} !Int
  , ifi_imcasts  :: {-# UNPACK #-} !Int
  , ifi_omcasts  :: {-# UNPACK #-} !Int
  , ifi_iqdrops  :: {-# UNPACK #-} !Int
  , ifi_name     :: !String
  , ifi_error    :: {-# UNPACK #-} !Int
#endif
  } deriving Show


#ifdef darwin_HOST_OS
instance Storable IfData where
  sizeOf _ = #{size ridley_if_data_t}
  alignment _ = #{alignment ridley_if_data_t}
  peek ptr = do
    ipackets <- (#peek ridley_if_data_t, ridley_ifi_ipackets) ptr
    opackets <- (#peek ridley_if_data_t, ridley_ifi_opackets) ptr
    ierrors  <- (#peek ridley_if_data_t, ridley_ifi_ierrors) ptr
    oerrors  <- (#peek ridley_if_data_t, ridley_ifi_oerrors) ptr
    ibytes   <- (#peek ridley_if_data_t, ridley_ifi_ibytes) ptr
    obytes   <- (#peek ridley_if_data_t, ridley_ifi_obytes) ptr
    imcasts  <- (#peek ridley_if_data_t, ridley_ifi_imcasts) ptr
    omcasts  <- (#peek ridley_if_data_t, ridley_ifi_omcasts) ptr
    iqdrops  <- (#peek ridley_if_data_t, ridley_ifi_iqdrops) ptr
    iname    <- (#peek ridley_if_data_t, ridley_ifi_name) ptr
    err      <- (#peek ridley_if_data_t, ridley_ifi_error) ptr
    return IfData {
      ifi_ipackets = ipackets
    , ifi_opackets = opackets
    , ifi_ierrors  = ierrors
    , ifi_oerrors  = oerrors
    , ifi_ibytes   = ibytes
    , ifi_obytes   = obytes
    , ifi_imcasts  = imcasts
    , ifi_omcasts  = omcasts
    , ifi_iqdrops  = iqdrops
    , ifi_name     = iname
    , ifi_error    = err
    }
  poke ptr IfData{..} = do
    (#poke ridley_if_data_t, ridley_ifi_ipackets) ptr ifi_ipackets
    (#poke ridley_if_data_t, ridley_ifi_opackets) ptr ifi_opackets
    (#poke ridley_if_data_t, ridley_ifi_ierrors)  ptr ifi_ierrors
    (#poke ridley_if_data_t, ridley_ifi_oerrors)  ptr ifi_oerrors
    (#poke ridley_if_data_t, ridley_ifi_ibytes)   ptr ifi_ibytes
    (#poke ridley_if_data_t, ridley_ifi_obytes)   ptr ifi_obytes
    (#poke ridley_if_data_t, ridley_ifi_imcasts)  ptr ifi_imcasts
    (#poke ridley_if_data_t, ridley_ifi_omcasts)  ptr ifi_omcasts
    (#poke ridley_if_data_t, ridley_ifi_iqdrops)  ptr ifi_iqdrops
    (#poke ridley_if_data_t, ridley_ifi_name)     ptr ifi_name
    (#poke ridley_if_data_t, ridley_ifi_error)    ptr ifi_error

ifDataCtx :: Context
ifDataCtx = mempty { ctxTypesTable = ridleyNetworkTypesTable }

ridleyNetworkTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
ridleyNetworkTypesTable = Map.fromList
  [ (C.TypeName "ridley_if_data_t", [t| IfData |])
  ]

#endif

--------------------------------------------------------------------------------
type NetworkMetrics = M.Map T.Text NetworkMetric

--------------------------------------------------------------------------------
data NetworkMetric = NetworkMetric {
    receive_packets     :: P.Gauge
  , transmit_packets    :: P.Gauge
  , receive_errs        :: P.Gauge
  , transmit_errs       :: P.Gauge
  , receive_bytes       :: P.Gauge
  , transmit_bytes      :: P.Gauge
  , receive_multicast   :: P.Gauge
  , transmit_multicast  :: P.Gauge
  , receive_drop        :: P.Gauge
  }
