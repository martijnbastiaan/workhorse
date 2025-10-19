{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{- | Lifted monadic interface for concurrent task execution using a worker pool.
This module provides a generalized version that works with any 'MonadBaseControl IO m'
monad, including monad transformer stacks. For a simpler 'IO'-only interface,
see "Control.Concurrent.Workhorse".
-}
module Control.Concurrent.Workhorse.Lifted (
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
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Coerce (coerce)
import Data.Default (Default (def))

import qualified Control.Concurrent.Workhorse.Internal as Internal

{- | Abstract worker pool type. Gets passed to workers and can be used to add
work dynamically. See 'addWork'. This is parameterized over the monad @m@ to
support monad transformer stacks.
-}
newtype Pool a b m
  = -- This is a new type wrapper around the internal Pool to avoid exposing
    -- the Internal module's types directly.
    Pool (Internal.Pool a b m)

{- | Run tasks concurrently using a worker pool. The results are in reverse order
of completion. Note that the output list can be much larger than the input list
if work is added dynamically (see 'addWork').

This lifted version works with any monad @m@ that has a 'MonadBaseControl IO'
instance, allowing you to use it with monad transformer stacks like
@ReaderT r IO@, @StateT s IO@, etc.
-}
doConcurrently ::
  (MonadBaseControl IO m, Traversable t) =>
  (Pool a b m -> a -> m b) ->
  t a ->
  m [b]
doConcurrently = doConcurrentlyWith def

{- | Like 'doConcurrently', but discards the results. Note that it is better to use this
function, than to use 'doConcurrently' and discard the results manually, as this avoids
building up results in memory.
-}
doConcurrently_ ::
  (MonadBaseControl IO m, Foldable t) =>
  (Pool a b m -> a -> m b) ->
  t a ->
  m ()
doConcurrently_ = doConcurrentlyWith_ def

-- | Like 'doConcurrently', but with custom configuration.
doConcurrentlyWith ::
  (MonadBaseControl IO m, Foldable t) =>
  Config ->
  (Pool a b m -> a -> m b) ->
  t a ->
  m [b]
doConcurrentlyWith cfg (coerce -> f) as =
  Internal.doConcurrentlyWith InternalConfig{collectResults = True} cfg f as

{- | Like 'doConcurrentlyWith', but discards the results. Note that it is better to use
this function, than to use 'doConcurrentlyWith' and discard the results manually, as
this avoids building up results in memory.
-}
doConcurrentlyWith_ ::
  (MonadBaseControl IO m, Foldable t) =>
  Config ->
  (Pool a b m -> a -> m b) ->
  t a ->
  m ()
doConcurrentlyWith_ cfg (coerce -> f) as =
  void $ Internal.doConcurrentlyWith InternalConfig{collectResults = False} cfg f as

-- | Add work to the worker pool dynamically.
addWork :: (MonadBaseControl IO m) => Pool a b m -> a -> m ()
addWork = Internal.addWork . coerce
