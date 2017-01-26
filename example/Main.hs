{-# LANGUAGE OverloadedStrings #-}
import           System.Metrics.Prometheus.Scott
import           Lens.Micro
import           Web.Spock
import           Web.Spock.Config
import           Network.Wai.Metrics
import           Control.Monad.Trans
import           Data.Monoid
import           Data.IORef
import           Katip
import           System.IO
import qualified Data.Text as T

spockWeb :: ScottCtx -> IO ()
spockWeb ctx = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg (app ctx))

app ctx = do
  case ctx ^. scottWaiMetrics of
    Nothing -> return ()
    Just m  -> middleware (metrics m)
  get root $ text "Hello World!"
  get "ping" $ text "pong"

main :: IO ()
main = do
    stdoutS <- mkHandleScribe ColorIfTerminal stdout InfoS V2
    let opts = newOptions [("service", "scott-test")] defaultMetrics
             & prometheusOptions . samplingFrequency .~ 5
             & dataRetentionPeriod .~ Just 60
             & katipScribes .~ ("ScottTest", [("stdout", stdoutS)])
    startScott opts ["metrics"] 8729 >>= spockWeb
