{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Ridley.Types (
    RidleyT(Ridley)
  , Ridley
  , runRidley
  , RidleyCtx(RidleyCtx)
  , ridleyThreadId
  , ridleyWaiMetrics
  , Port
  , PrometheusOptions
  , RidleyMetric(..)
  , RidleyOptions
  , RidleyMetricHandler(..)
  , defaultMetrics
  , newOptions
  , prometheusOptions
  , ridleyMetrics
  , katipScribes
  , katipSeverity
  , dataRetentionPeriod
  , runHandler
  ) where

import           Control.Concurrent (ThreadId)
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time
import           Katip
import           Lens.Micro.TH
import           Network.Wai.Metrics (WaiMetrics)
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Remote.Monitoring.Prometheus

--------------------------------------------------------------------------------
type Port = Int
type PrometheusOptions = AdapterOptions

--------------------------------------------------------------------------------
data RidleyMetricHandler = forall c. RidleyMetricHandler {
    metric       :: c
  , updateMetric :: c -> Bool -> IO ()
  , flush        :: !Bool
  -- ^Whether or net to flush this Metric
  }

--------------------------------------------------------------------------------
data RidleyMetric = ProcessMemory
                  | CPULoad
                  | GHCConc
                  -- ^ Tap into the metrics exposed by GHC.Conc
                  | Network
                  | Wai
                  | DiskUsage
                  -- ^ Gets stats about Disk usage (free space, etc)
                  | CustomMetric T.Text (forall m. MonadIO m => RidleyOptions -> P.RegistryT m RidleyMetricHandler)
                  -- ^ A user-defined metric, identified by a name.

instance Show RidleyMetric where
  show ProcessMemory         = "ProcessMemory"
  show CPULoad               = "CPULoad"
  show GHCConc               = "GHCConc"
  show Network               = "Network"
  show Wai                   = "Wai"
  show DiskUsage             = "DiskUsage"
  show (CustomMetric name _) = "Custom@" <> T.unpack name

instance Eq RidleyMetric where
  (==) ProcessMemory ProcessMemory             = True
  (==) CPULoad CPULoad                         = True
  (==) GHCConc GHCConc                         = True
  (==) Network Network                         = True
  (==) Wai     Wai                             = True
  (==) DiskUsage DiskUsage                     = True
  (==) (CustomMetric n1 _) (CustomMetric n2 _) = (==) n1 n2
  (==) _ _                                     = False

instance Ord RidleyMetric where
  compare ProcessMemory xs = case xs of
    ProcessMemory          -> EQ
    _                      -> GT
  compare CPULoad xs       = case xs of
    ProcessMemory          -> LT
    CPULoad                -> EQ
    _                      -> GT
  compare GHCConc xs       = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> EQ
    _                      -> GT
  compare Network xs       = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> LT
    Network                -> EQ
    _                      -> GT
  compare Wai     xs       = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> LT
    Network                -> LT
    Wai                    -> EQ
    _                      -> GT
  compare DiskUsage xs     = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> LT
    Network                -> LT
    Wai                    -> LT
    DiskUsage              -> EQ
    _                      -> GT
  compare (CustomMetric n1 _) xs = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> LT
    Network                -> LT
    Wai                    -> LT
    DiskUsage              -> LT
    (CustomMetric n2 _)    -> compare n1 n2

--------------------------------------------------------------------------------
data RidleyOptions = RidleyOptions {
    _prometheusOptions :: PrometheusOptions
  , _ridleyMetrics :: Set.Set RidleyMetric
  , _katipScribes :: (Katip.Namespace, [(T.Text, Katip.Scribe)])
  , _katipSeverity :: Katip.Severity
  , _dataRetentionPeriod :: Maybe NominalDiffTime
  -- ^ How much to retain the data, in seconds.
  -- Pass `Nothing` to not flush the metrics.
  }

makeLenses ''RidleyOptions

--------------------------------------------------------------------------------
defaultMetrics :: [RidleyMetric]
defaultMetrics = [ProcessMemory, CPULoad, GHCConc, Network, Wai, DiskUsage]

--------------------------------------------------------------------------------
newOptions :: [(T.Text, T.Text)]
           -> [RidleyMetric]
           -> RidleyOptions
newOptions appLabels metrics = RidleyOptions {
    _prometheusOptions = defaultOptions (P.fromList appLabels)
  , _ridleyMetrics     = Set.fromList metrics
  , _katipSeverity     = InfoS
  , _katipScribes      = mempty
  , _dataRetentionPeriod = Nothing
  }

--------------------------------------------------------------------------------
runHandler :: RidleyMetricHandler -> IO ()
runHandler (RidleyMetricHandler m u f) = u m f

--------------------------------------------------------------------------------
newtype RidleyT t a = Ridley { unRidley :: ReaderT RidleyOptions t a }
  deriving (Functor, Applicative, Monad, MonadReader RidleyOptions, MonadIO, MonadTrans)

type Ridley = RidleyT (P.RegistryT (KatipT IO))

data RidleyCtx = RidleyCtx {
    _ridleyThreadId   :: ThreadId
  , _ridleyWaiMetrics :: Maybe WaiMetrics
  }

makeLenses ''RidleyCtx

instance Katip Ridley where
  getLogEnv = Ridley $ lift (lift getLogEnv)
  localLogEnv = localLogEnv

instance KatipContext Ridley where
  getKatipContext   = return mempty
  localKatipContext = localKatipContext
  getKatipNamespace = _logEnvApp <$> Ridley (lift $ lift (getLogEnv))
  localKatipNamespace = localKatipNamespace

--------------------------------------------------------------------------------
runRidley :: RidleyOptions -> LogEnv -> Ridley a -> IO a
runRidley opts le ridley = (runReaderT $ unKatipT $ P.evalRegistryT $ (runReaderT $ unRidley ridley) opts) le
