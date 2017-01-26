{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Metrics.Prometheus.Scott.Types (
    ScottT(Scott)
  , Scott
  , runScott
  , ScottCtx(ScottCtx)
  , scottThreadId
  , scottWaiMetrics
  , Port
  , PrometheusOptions
  , ScottMetric(..)
  , ScottOptions
  , ScottMetricHandler(..)
  , defaultMetrics
  , newOptions
  , prometheusOptions
  , scottMetrics
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
data ScottMetric = ProcessMemory
                 | CPULoad
                 | GHCConc
                 -- ^ Tap into the metrics exposed by GHC.Conc
                 | Network
                 | Wai
                 | DiskUsage
                 -- ^ Gets stats about Disk usage (free space, etc)
                 deriving (Show, Ord, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
data ScottOptions = ScottOptions {
    _prometheusOptions :: PrometheusOptions
  , _scottMetrics :: Set.Set ScottMetric
  , _katipScribes :: (Katip.Namespace, [(T.Text, Katip.Scribe)])
  , _katipSeverity :: Katip.Severity
  , _dataRetentionPeriod :: Maybe NominalDiffTime
  -- ^ How much to retain the data, in seconds.
  -- Pass `Nothing` to not flush the metrics.
  }

makeLenses ''ScottOptions

--------------------------------------------------------------------------------
defaultMetrics :: [ScottMetric]
defaultMetrics = [minBound .. maxBound]

--------------------------------------------------------------------------------
newOptions :: [(T.Text, T.Text)]
           -> [ScottMetric]
           -> ScottOptions
newOptions appLabels metrics = ScottOptions {
    _prometheusOptions = defaultOptions (P.fromList appLabels)
  , _scottMetrics   = Set.fromList metrics
  , _katipSeverity  = InfoS
  , _katipScribes   = mempty
  , _dataRetentionPeriod = Nothing
  }

--------------------------------------------------------------------------------
data ScottMetricHandler = forall c. ScottMetricHandler {
    metric       :: c
  , updateMetric :: c -> Bool -> IO ()
  , flush        :: !Bool
  -- ^Whether or net to flush this Metric
  }

--------------------------------------------------------------------------------
runHandler :: ScottMetricHandler -> IO ()
runHandler (ScottMetricHandler m u f) = u m f

--------------------------------------------------------------------------------
newtype ScottT t a = Scott { unScott :: ReaderT ScottOptions t a }
  deriving (Functor, Applicative, Monad, MonadReader ScottOptions, MonadIO, MonadTrans)

type Scott = ScottT (P.RegistryT (KatipT IO))

data ScottCtx = ScottCtx {
    _scottThreadId   :: ThreadId
  , _scottWaiMetrics :: Maybe WaiMetrics
  }

makeLenses ''ScottCtx

instance Katip Scott where
  getLogEnv = Scott $ lift (lift getLogEnv)

instance KatipContext Scott where
  getKatipContext   = return mempty
  getKatipNamespace = _logEnvApp <$> Scott (lift $ lift (getLogEnv))

--------------------------------------------------------------------------------
runScott :: ScottOptions -> LogEnv -> Scott a -> IO a
runScott opts le scott = (runReaderT $ unKatipT $ P.evalRegistryT $ (runReaderT $ unScott scott) opts) le
