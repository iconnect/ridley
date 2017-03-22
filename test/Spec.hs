{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Concurrent
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Ord
import           Lens.Micro
import           System.Metrics as EKG
import           System.Metrics.Prometheus.Registry
import           System.Metrics.Prometheus.Ridley
import           System.Metrics.Prometheus.Ridley.Types
import           System.Remote.Monitoring.Prometheus (toPrometheusRegistry)
import           Test.Tasty
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [unitTests]

--------------------------------------------------------------------------------
startRidleyWith :: [RidleyMetric] -> IO (IO Registry, RidleyCtx)
startRidleyWith metrics = do
  store <- EKG.newStore
  let opts = newOptions [("service", "ridley-test")] metrics
  ctx <- startRidleyWithStore opts ["metrics"] 8977 store
  return $ (toPrometheusRegistry store (opts ^. prometheusOptions), ctx)

--------------------------------------------------------------------------------
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ withResource (startRidleyWith []) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with empty metrics yield an empty store" $ do
        (getRegistry, _) <- setupFn
        r <- getRegistry >>= sample
        Map.null (unRegistrySample r) @?= True
  ]
