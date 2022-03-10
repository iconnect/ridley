{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  , RidleyMetricHandler
  , metric
  , updateMetric
  , flush
  , label
  , mkRidleyMetricHandler
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
import           GHC.Stack
import           Katip
import           Lens.Micro.TH
import           Network.Wai.Metrics (WaiMetrics)
import qualified System.Metrics.Prometheus.MetricId as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Remote.Monitoring.Prometheus
import           System.Metrics.Prometheus.Ridley.Types.Internal

--------------------------------------------------------------------------------
type Port = Int
type PrometheusOptions = AdapterOptions

mkRidleyMetricHandler :: forall c. HasCallStack
                      => T.Text
                      -> c -> (c -> Bool -> IO ()) -> Bool -> RidleyMetricHandler
mkRidleyMetricHandler lbl c runC flsh = withFrozenCallStack $ RidleyMetricHandler {
    metric       = c
  , updateMetric = runC
  , flush        = flsh
  , label        = lbl
  , _cs          = popCallStack callStack
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
                  | CustomMetric !T.Text
                                 -- ^ The name of the metric
                                 !(Maybe Int)
                                 -- ^ An optional timeout, in microseconds,
                                 -- that regulates how often the metric is
                                 -- actually updated. If Nothing, the metric
                                 -- will be updated using Ridley top-level setting,
                                 -- if 'Just' the underlying 'IO' action will be run
                                 -- only every @n@ seconds, or cached otherwise.
                                 (forall m. MonadIO m => RidleyOptions -> P.RegistryT m RidleyMetricHandler)
                                 -- ^ An action to generate the handler.
                  -- ^ A user-defined metric, identified by a name.

instance Show RidleyMetric where
  show ProcessMemory         = "ProcessMemory"
  show CPULoad               = "CPULoad"
  show GHCConc               = "GHCConc"
  show Network               = "Network"
  show Wai                   = "Wai"
  show DiskUsage             = "DiskUsage"
  show (CustomMetric name _ _) = "Custom@" <> T.unpack name

instance Eq RidleyMetric where
  (==) ProcessMemory ProcessMemory             = True
  (==) CPULoad CPULoad                         = True
  (==) GHCConc GHCConc                         = True
  (==) Network Network                         = True
  (==) Wai     Wai                             = True
  (==) DiskUsage DiskUsage                     = True
  (==) (CustomMetric n1 _ _) (CustomMetric n2 _ _) = (==) n1 n2
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
  compare (CustomMetric n1 _ _) xs = case xs of
    ProcessMemory          -> LT
    CPULoad                -> LT
    GHCConc                -> LT
    Network                -> LT
    Wai                    -> LT
    DiskUsage              -> LT
    (CustomMetric n2 _ _)    -> compare n1 n2

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
runHandler (RidleyMetricHandler m u f _ _) = u m f

--------------------------------------------------------------------------------
newtype RidleyT t a = Ridley { _unRidley :: ReaderT RidleyOptions t a }
  deriving (Functor, Applicative, Monad, MonadReader RidleyOptions, MonadIO, MonadTrans)

type Ridley = RidleyT (P.RegistryT (KatipContextT IO))

data RidleyCtx = RidleyCtx {
    _ridleyThreadId   :: ThreadId
  , _ridleyWaiMetrics :: Maybe WaiMetrics
  }

makeLenses ''RidleyCtx

instance Katip Ridley where
  getLogEnv = Ridley $ lift (lift getLogEnv)
  localLogEnv f (Ridley (ReaderT m)) =
    Ridley $ ReaderT $ \env -> P.RegistryT (localLogEnv f $ P.unRegistryT (m env))

instance KatipContext Ridley where
  getKatipContext   = return mempty
  getKatipNamespace = _logEnvApp <$> Ridley (lift $ lift (getLogEnv))
  localKatipContext f (Ridley (ReaderT m)) =
    Ridley $ ReaderT $ \env -> P.RegistryT (localKatipContext f $ P.unRegistryT (m env))
  localKatipNamespace f (Ridley (ReaderT m)) =
    Ridley $ ReaderT $ \env -> P.RegistryT (localKatipNamespace f $ P.unRegistryT (m env))

--------------------------------------------------------------------------------
runRidley :: RidleyOptions -> LogEnv -> Ridley a -> IO a
runRidley opts le (Ridley ridley) =
  (runKatipContextT le (mempty :: SimpleLogPayload) mempty $ P.evalRegistryT $ (runReaderT ridley) opts)
