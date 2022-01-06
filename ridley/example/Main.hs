{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import           System.Metrics.Prometheus.Ridley
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           Lens.Micro
import           Web.Spock
import           Web.Spock.Config
import           Network.Wai.Metrics
import           Control.Monad.Trans
import           Data.Monoid
import           Data.Time.Clock.POSIX
import           Data.IORef
import           Katip
import           System.IO
import qualified Data.Text as T

spockWeb :: RidleyCtx -> IO ()
spockWeb ctx = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg (app ctx))

app ctx = do
  case ctx ^. ridleyWaiMetrics of
    Nothing -> return ()
    Just m  -> middleware (metrics m)
  get root $ text "Hello World!"
  get "ping" $ text "pong"

customExpensiveMetric :: RidleyMetric
customExpensiveMetric =
  CustomMetric "my-expensive" (Just 60) get_metric
  where
    get_metric :: MonadIO m => RidleyOptions -> P.RegistryT m RidleyMetricHandler
    get_metric opts = do
        m <- P.registerGauge "current_time" (opts ^. prometheusOptions . labels)
        return RidleyMetricHandler { metric = m, updateMetric = update, flush = False }

    update :: P.Gauge -> Bool -> IO ()
    update gauge _ = do n  <- getPOSIXTime
                        tn <- getCurrentTime
                        putStrLn $ "Updating time, at " <> show tn
                        P.set (realToFrac n) gauge

main :: IO ()
main = do
#if MIN_VERSION_katip(0,8,0)
    let onlyErrors i = pure $ Katip._itemSeverity i >= Katip.ErrorS
    ridleyScribe <-
      Katip.mkHandleScribe Katip.ColorIfTerminal stdout onlyErrors Katip.V2
#else
    ridleyScribe <-
      Katip.mkHandleScribe Katip.ColorIfTerminal stdout Katip.ErrorS Katip.V2
#endif
    let opts = newOptions [("service", "ridley-test")] (customExpensiveMetric : defaultMetrics)
             & prometheusOptions . samplingFrequency .~ 5
             & dataRetentionPeriod .~ Just 60
             & katipScribes .~ ("RidleyTest", [("stdout", ridleyScribe)])
    startRidley opts ["metrics"] 8729 >>= spockWeb
