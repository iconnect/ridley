{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
import           System.Metrics.Prometheus.Ridley
import qualified System.Metrics.Prometheus.Metric.Gauge as P
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as P
import           System.Metrics.Prometheus.Ridley.Types
import           Lens.Micro
import           Network.Wai.Metrics
import           Network.HTTP.Types (status200)
import           Control.Exception
import           Control.Monad.Trans
import           Data.Time.Clock.POSIX
import           Katip
import           System.IO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad.Reader
import           System.Random.Stateful

webApp :: RidleyCtx -> IO ()
webApp ctx = Warp.run 8080 (app ctx)

app :: RidleyCtx -> Wai.Application
app ctx = do
  case ctx ^. ridleyWaiMetrics of
    Nothing -> myApp
    Just m  -> (metrics m) myApp
  where
    myApp _rq respond =
      respond $ Wai.responseLBS status200 [] "Hello World"

customExpensiveMetric :: RidleyMetric
customExpensiveMetric =
  CustomMetric "my-expensive" (Just $ 60 * 1_000_000) get_metric
  where
    get_metric :: Ridley RidleyMetricHandler
    get_metric = do
        opts <- ask
        m <- lift $ P.registerGauge "current_time" (opts ^. prometheusOptions . labels)
        return $ mkRidleyMetricHandler "current_time" m update False

    update :: P.Gauge -> Bool -> IO ()
    update gauge _ = do n  <- getPOSIXTime
                        tn <- getCurrentTime
                        putStrLn $ "Updating time, at " <> show tn
                        P.set (realToFrac n) gauge

customCrashfulMetric :: RidleyMetric
customCrashfulMetric =
  CustomMetric "my-crashful" (Just $ 60 * 1_000_000) get_metric
  where
    get_metric :: Ridley RidleyMetricHandler
    get_metric = do
        opts <- ask
        m <- lift $ P.registerGauge "crashful" (opts ^. prometheusOptions . labels)
        return $ mkRidleyMetricHandler "crashful" m (\_ _ -> throwIO $ userError "CRASH!!") False

customFlakyMetric :: RidleyMetric
customFlakyMetric =
  CustomMetric "my-flaky" (Just $ 10 * 1_000_000) get_metric
  where
    get_metric :: Ridley RidleyMetricHandler
    get_metric = do
        opts <- ask
        m <- lift $ P.registerGauge "flaky" (opts ^. prometheusOptions . labels)
        return $ mkRidleyMetricHandler "flaky" m updateFlaky False

updateFlaky :: P.Gauge -> Bool -> IO ()
updateFlaky g _ = do
  now <- randomRIO (1, 100)
  case now `mod` 2 == 0 of
    True -> P.set (realToFrac now) g
    False -> throwIO $ userError $ "FLAKY! Dice was " <> show @Int now

main :: IO ()
main = do
#if MIN_VERSION_katip(0,8,0)
    let onlyErrors i = pure $ Katip._itemSeverity i >= Katip.DebugS
    ridleyScribe <-
      Katip.mkHandleScribe Katip.ColorIfTerminal stdout onlyErrors Katip.V2
#else
    ridleyScribe <-
      Katip.mkHandleScribe Katip.ColorIfTerminal stdout Katip.DebugS Katip.V2
#endif
    let opts = newOptions [("service", "ridley-test")] (
                 customExpensiveMetric : customCrashfulMetric : customFlakyMetric : defaultMetrics
                 )
             & prometheusOptions . samplingFrequency .~ 5
             & dataRetentionPeriod .~ Just 60
             & katipScribes .~ ("RidleyTest", [("stdout", ridleyScribe)])
    startRidley opts ["metrics"] 8729 >>= webApp
