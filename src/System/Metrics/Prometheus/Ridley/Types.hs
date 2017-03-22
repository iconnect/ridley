{-# LANGUAGE ExistentialQuantification #-}
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
data RidleyMetric = ProcessMemory
                 | CPULoad
                 | GHCConc
                 -- ^ Tap into the metrics exposed by GHC.Conc
                 | Network
                 | Wai
                 | DiskUsage
                 -- ^ Gets stats about Disk usage (free space, etc)
                 deriving (Show, Ord, Eq, Enum, Bounded)

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
defaultMetrics = [minBound .. maxBound]

--------------------------------------------------------------------------------
newOptions :: [(T.Text, T.Text)]
           -> [RidleyMetric]
           -> RidleyOptions
newOptions appLabels metrics = RidleyOptions {
    _prometheusOptions = defaultOptions (P.fromList appLabels)
  , _ridleyMetrics   = Set.fromList metrics
  , _katipSeverity  = InfoS
  , _katipScribes   = mempty
  , _dataRetentionPeriod = Nothing
  }

--------------------------------------------------------------------------------
data RidleyMetricHandler = forall c. RidleyMetricHandler {
    metric       :: c
  , updateMetric :: c -> Bool -> IO ()
  , flush        :: !Bool
  -- ^Whether or net to flush this Metric
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

instance KatipContext Ridley where
  getKatipContext   = return mempty
  getKatipNamespace = _logEnvApp <$> Ridley (lift $ lift (getLogEnv))

--------------------------------------------------------------------------------
runRidley :: RidleyOptions -> LogEnv -> Ridley a -> IO a
runRidley opts le ridley = (runReaderT $ unKatipT $ P.evalRegistryT $ (runReaderT $ unRidley ridley) opts) le
