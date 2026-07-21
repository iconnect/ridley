{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.IORef
import           Data.List
import           Data.Maybe (isJust)
import           Data.Monoid
import           Data.Ord
import           Data.String.Conv
import           Lens.Micro
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import           System.IO.Unsafe (unsafePerformIO)
import           System.Metrics as EKG
import           System.Metrics.Prometheus.Concurrent.Registry
import           System.Metrics.Prometheus.Registry (unRegistrySample)
import           System.Metrics.Prometheus.Ridley
import           System.Metrics.Prometheus.Ridley.Metrics.DiskUsage
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
containsMetrics port keys = go 3
  where
    go !attempts = do
      request  <- HTTP.parseRequest $ "http://localhost:" <> show port <> "/metrics"
      (response :: Either SomeException (HTTP.Response ByteString)) <- try (HTTP.httpLbs request ridleyManager)
      case response of
        Left e -> if attempts <= 0 then throwIO e else threadDelay (2 * 10^6) >> go (attempts - 1)
        Right res -> do
          let haystack = toS $ HTTP.responseBody res
          forM_ keys $ \key -> do
            assertBool (T.unpack $ "Key " <> key <> " was not found in \"" <> haystack <> "\"") (key `T.isInfixOf` haystack)

--------------------------------------------------------------------------------
-- | Like 'containsMetrics', but polls until the keys appear (or the deadline
-- expires), to accommodate metrics which are registered asynchronously by the
-- update loop rather than at boot.
eventuallyContainsMetrics :: Port -> [T.Text] -> Assertion
eventuallyContainsMetrics port keys = go 15
  where
    go :: Int -> Assertion
    go !attempts = do
      request  <- HTTP.parseRequest $ "http://localhost:" <> show port <> "/metrics"
      (response :: Either SomeException (HTTP.Response ByteString)) <- try (HTTP.httpLbs request ridleyManager)
      case response of
        Left e -> if attempts <= 0 then throwIO e else threadDelay (10^6) >> go (attempts - 1)
        Right res -> do
          let haystack = toS $ HTTP.responseBody res
          case all (`T.isInfixOf` haystack) keys of
            True  -> return ()
            False
              | attempts <= 0 ->
                  assertBool (T.unpack $ "Keys " <> T.intercalate "," keys
                             <> " were not found in \"" <> haystack <> "\"") False
              | otherwise -> threadDelay (10^6) >> go (attempts - 1)

--------------------------------------------------------------------------------
-- | Regression test for iconnect/hermes#2596: a transient failure of the
-- disk-stats runner at boot (e.g. @posix_spawnp: failed (Bad address)@ when
-- shelling out to @df@) must not permanently disable disk metrics. The
-- handler is expected to self-heal once the runner recovers, registering the
-- gauges from the update loop.
startFlakyDiskRidley :: Port -> IO (IORef Int, RidleyCtx)
startFlakyDiskRidley port = do
  store <- EKG.newStore
  calls <- newIORef (0 :: Int)
  let flakyStats = do
        n <- atomicModifyIORef' calls (\c -> (c + 1, c))
        if n < 2
          then throwIO (userError "posix_spawnp: failed (Bad address)")
          else pure [ DiskStats { _diskFilesystem  = "/dev/testfs0"
                                , _diskUsedBytes   = 1000
                                , _diskUsedPercent = 10
                                , _diskFreeBytes   = 9000
                                , _diskFreePercent = 90
                                } ]
  let opts = newOptions [("service", "ridley-test")]
                        [CustomMetric "disk-usage-flaky" Nothing (newDiskUsageMetricsWith flakyStats)]
               & prometheusOptions . samplingFrequency .~ 1
  ctx <- startRidleyWithStore opts ["metrics"] port store
  return (calls, ctx)

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

  , withResource (startFlakyDiskRidley 8705) (\(_, ctx) -> killThread (ctx ^. ridleyThreadId)) $ \setupFn -> do
      testCase "Disk metrics self-heal after a transient boot-time failure (hermes#2596)" $ do
        (calls, _) <- setupFn
        eventuallyContainsMetrics 8705 [ "disk_used_bytes_blocks{filesystem=\"/dev/testfs0\""
                                       , "disk_free_bytes_blocks{filesystem=\"/dev/testfs0\""
                                       ]
        totalCalls <- readIORef calls
        assertBool "runner should have been retried past the initial failures" (totalCalls >= 2)
  ]
