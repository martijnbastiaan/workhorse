{-# LANGUAGE ViewPatterns #-}

{- | Simple interface for concurrent task execution using a worker pool. For a lifted
monadic interface, see "Control.Concurrent.Workhorse.Lifted".
-}
module Control.Concurrent.Workhorse (
  Config (..),
  Pool,
  doConcurrently,
  doConcurrently_,
  doConcurrentlyWith,
  doConcurrentlyWith_,
  addWork,
) where

import Control.Concurrent.Workhorse.Internal (Config (..), InternalConfig (..))
import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Default (Default (def))

import qualified Control.Concurrent.Workhorse.Internal as Internal

{- | Abstract worker pool type. Gets passed to workers and can be used to add
work dynamically. See 'addWork'.
-}
newtype Pool a b
  = -- This is a new type wrapper around the internal Pool to avoid exposing
    -- the Internal module's types directly.
    Pool (Internal.Pool a b IO)

{- | Run tasks concurrently using a worker pool. The results are in reverse order
of completion. Note that the output list can be much larger than the input list
if work is added dynamically (see 'addWork').
-}
doConcurrently :: (Pool a b -> a -> IO b) -> [a] -> IO [b]
doConcurrently = doConcurrentlyWith def

{- | Like 'doConcurrently', but discards the results. Note that it is better to use this
function, than to use 'doConcurrently' and discard the results manually, as this avoids
building up results in memory.
-}
doConcurrently_ :: (Pool a b -> a -> IO b) -> [a] -> IO ()
doConcurrently_ = doConcurrentlyWith_ def

-- | Like 'doConcurrently', but with custom configuration.
doConcurrentlyWith :: Config -> (Pool a b -> a -> IO b) -> [a] -> IO [b]
doConcurrentlyWith cfg (coerce -> f) as =
  Internal.doConcurrentlyWith InternalConfig{collectResults = True} cfg f as

{- | Like 'doConcurrentlyWith', but discards the results. Note that it is better to use
this function, than to use 'doConcurrentlyWith' and discard the results manually, as
this avoids building up results in memory.
-}
doConcurrentlyWith_ :: Config -> (Pool a b -> a -> IO b) -> [a] -> IO ()
doConcurrentlyWith_ cfg (coerce -> f) as =
  void $ Internal.doConcurrentlyWith InternalConfig{collectResults = False} cfg f as

-- | Add work to the worker pool dynamically.
addWork :: Pool a b -> a -> IO ()
addWork = Internal.addWork . coerce
