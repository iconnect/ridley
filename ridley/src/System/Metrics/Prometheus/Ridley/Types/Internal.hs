{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Metrics.Prometheus.Ridley.Types.Internal
  ( RidleyMetricHandler(..)
  , Logger
  ) where

import           Katip
import           GHC.Stack
import qualified Data.Text as T

--------------------------------------------------------------------------------
data RidleyMetricHandler = forall c. RidleyMetricHandler {
  -- | An opaque metric
    metric       :: c
  -- | An IO action used to update the metric
  , updateMetric :: c -> Bool -> IO ()
  -- | Whether or not to flush this Metric
  , flush        :: !Bool
  -- | A user-friendly label, used to report errors
  , label        :: !T.Text
  -- | A CallStack, for precise error reporting
  , _cs          :: CallStack
  }

type Logger = Severity -> T.Text -> IO ()
