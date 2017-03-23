{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Ord
import           Data.String.Conv
import qualified Data.Text as T
import           Lens.Micro
import qualified Network.HTTP.Client as HTTP
import           System.IO.Unsafe (unsafePerformIO)
import           System.Metrics as EKG
import           System.Metrics.Prometheus.Registry
import           System.Metrics.Prometheus.Ridley
import           System.Metrics.Prometheus.Ridley.Types
import           System.Remote.Monitoring.Prometheus (toPrometheusRegistry)
import           Test.Tasty
import           Test.Tasty.HUnit

ridleyManager :: HTTP.Manager
ridleyManager = unsafePerformIO $ HTTP.newManager HTTP.defaultManagerSettings
{-# NOINLINE ridleyManager #-}

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [unitTests]

--------------------------------------------------------------------------------
startRidleyWith :: Port -> [RidleyMetric] -> IO (IO Registry, RidleyCtx)
startRidleyWith port metrics = do
  store <- EKG.newStore
  let opts = newOptions [("service", "ridley-test")] metrics
  ctx <- startRidleyWithStore opts ["metrics"] port store
  return $ (toPrometheusRegistry store (opts ^. prometheusOptions), ctx)

--------------------------------------------------------------------------------
containsMetric :: Port -> T.Text -> Assertion
containsMetric port key = containsMetrics port [key]

--------------------------------------------------------------------------------
containsMetrics :: Port -> [T.Text] -> Assertion
containsMetrics port keys = do
  request <- HTTP.parseRequest $ "http://localhost:" <> show port <> "/metrics"
  response <- HTTP.httpLbs request ridleyManager
  let haystack = toS $ HTTP.responseBody response
  forM_ keys $ \key -> do
    assertBool (T.unpack $ "Key " <> key <> " was not found in \"" <> haystack <> "\"") (key `T.isInfixOf` haystack)

--------------------------------------------------------------------------------
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ withResource (startRidleyWith 8700 []) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with empty metrics yield an empty store" $ do
        (getRegistry, _) <- setupFn
        r <- getRegistry >>= sample
        Map.null (unRegistrySample r) @?= True

  , withResource (startRidleyWith 8701 [Wai]) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with wai metrics populates the store & ctx" $ do
        (getRegistry, ctx) <- setupFn
        isJust (ctx ^. ridleyWaiMetrics) @?= True
        r <- getRegistry >>= sample
        Map.null (unRegistrySample r) @?= False
        containsMetrics 8701 [ "# TYPE wai_request_count counter"
                             , "# TYPE wai_response_status_1xx counter"
                             , "# TYPE wai_response_status_2xx counter"
                             , "# TYPE wai_response_status_3xx counter"
                             , "# TYPE wai_response_status_4xx counter"
                             , "# TYPE wai_response_status_5xx counter"
                             ]

  , withResource (startRidleyWith 8702 [Network]) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with network metrics populates the store" $ do
        (getRegistry, _) <- setupFn
        containsMetrics 8702 [ "# TYPE network_receive_bytes gauge"
                             , "# TYPE network_receive_drop gauge"
                             , "# TYPE network_receive_errs gauge"
                             , "# TYPE network_receive_multicast gauge"
                             , "# TYPE network_receive_packets gauge"
                             , "# TYPE network_transmit_bytes gauge"
                             , "# TYPE network_transmit_errs gauge"
                             , "# TYPE network_transmit_multicast gauge"
                             , "# TYPE network_transmit_packets gauge"
                             ]

  , withResource (startRidleyWith 8703 [ProcessMemory]) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with process memory metrics populates the store" $ do
        (getRegistry, _) <- setupFn
        containsMetrics 8703 ["# TYPE process_memory_kb gauge"]

  , withResource (startRidleyWith 8706 [DiskUsage]) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with Disk Usage metrics populates the store" $ do
        (getRegistry, _) <- setupFn
        containsMetrics 8706 [ "# TYPE disk_free_bytes_blocks gauge"
                             , "# TYPE disk_used_bytes_blocks gauge"
                             ]

  , withResource (startRidleyWith 8704 [CPULoad]) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Starting Ridley with CPU Load metrics populates the store" $ do
        (getRegistry, _) <- setupFn
        containsMetrics 8704 [ "# TYPE cpu_load1 gauge"
                             , "# TYPE cpu_load15 gauge"
                             , "# TYPE cpu_load5 gauge"
                             ]


  ]
