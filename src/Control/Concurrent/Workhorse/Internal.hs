{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Concurrent.Workhorse.Internal where

import Control.Concurrent (forkIO, getNumCapabilities, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
#if MIN_VERSION_base(4,20,0)
import Control.Exception (ExceptionWithContext, SomeException)
#else
import Control.Exception (SomeException)
#endif
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Control (MonadBaseControl (StM, liftBaseWith, restoreM))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList))

import qualified Control.Exception

#if MIN_VERSION_base(4,20,0)
type ExceptionWithSomeException = ExceptionWithContext SomeException
#else
type ExceptionWithSomeException = SomeException
#endif

-- | Worker pool configuration
data Config = Config
  { nWorkers :: Maybe Int
  {- ^ Maximum number of worker threads. If 'Nothing', defaults to the number of
  capabilities of the runtime system. See 'getNumCapabilities'.
  -}
  }

instance Default Config where
  def = Config{nWorkers = Nothing}

data InternalConfig = InternalConfig
  { collectResults :: Bool
  {- ^ Whether to collect results from workers. This is set to 'False' for \"void\"
  operations to prevent storing result that won't be used.
  -}
  }

data Pool a b m = Pool
  { events :: Chan (Event a b m)
  , maxWorkers :: Int
  }

data Event a b m
  = WorkerFinished (StM m b)
  | WorkerFinishedWithError ExceptionWithSomeException
  | AddWork a

data PoolState a b = PoolState
  { workersActive :: Int
  , queue :: [a]
  , results :: [b]
  }

liftM :: (MonadBaseControl b m) => b a -> m a
liftM f = liftBaseWith $ \_ -> f

-- | Monadic version of @unless@, taking the condition in the monad
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM condM acc = do
  cond <- condM
  unless cond acc

whileM :: (Monad m) => m Bool -> m ()
whileM act = do
  b <- act
  when b $ whileM act

doConcurrentlyWith ::
  (MonadBaseControl IO m, Foldable t) =>
  InternalConfig ->
  Config ->
  (Pool a b m -> a -> m b) ->
  t a ->
  m [b]
doConcurrentlyWith InternalConfig{collectResults} Config{nWorkers} f as = do
  nThreads <- liftM $ maybe getNumCapabilities pure nWorkers
  events <- liftM $ newChan

  let
    pool = Pool{events = events, maxWorkers = nThreads}
    initialState = PoolState{workersActive = 0, queue = toList as, results = []}

  evalStateT (loop collectResults pool (wrapWorker events (f pool))) initialState

loop ::
  (MonadBaseControl IO m) =>
  Bool ->
  Pool a b m ->
  (a -> m ()) ->
  StateT (PoolState a b) m [b]
loop collectResults Pool{events, maxWorkers} wrappedWorker = whileM trySpawnWorker >> go
 where
  go = do
    event <- lift $ liftM $ readChan events

    case event of
      AddWork a -> addToQueue a
      WorkerFinished stmResult -> do
        result <- lift $ restoreM stmResult
        when collectResults $ addResult result
        modifyWorkersActive pred
      WorkerFinishedWithError err -> do
        modifyWorkersActive pred
        waitAndRethrow err

    whileM trySpawnWorker
    shouldTerminate <- shouldTerminateM
    if shouldTerminate
      then do
        PoolState{results} <- get
        pure results
      else go

  trySpawnWorker = do
    allWorkersActive <- allWorkersActiveM
    if allWorkersActive
      then
        pure False
      else do
        maybeA <- popQueue
        case maybeA of
          Just work -> do
            void $ lift $ liftBaseWith $ \runInIO ->
              forkIO (void $ runInIO (wrappedWorker work))
            modifyWorkersActive succ
            pure True
          Nothing ->
            pure False

  allWorkersActiveM = do
    PoolState{workersActive} <- get
    pure $ workersActive >= maxWorkers

  shouldTerminateM = do
    PoolState{workersActive, queue} <- get
    pure $ workersActive == 0 && null queue

  modifyWorkersActive f =
    modify $ \case
      PoolState{workersActive, queue, results} ->
        PoolState{workersActive = f workersActive, queue, results}

  addResult result =
    modify $ \case
      PoolState{workersActive, queue, results} ->
        PoolState{workersActive, queue, results = result : results}

  addToQueue a =
    modify $ \case
      PoolState{workersActive, queue, results} ->
        PoolState{workersActive, queue = a : queue, results}

  popQueue = do
    st <- get
    case st of
      PoolState{workersActive, queue, results} ->
        case queue of
          (work : rest) -> do
            modify $ \_ -> PoolState{workersActive, queue = rest, results}
            pure (Just work)
          [] -> pure Nothing

  waitAndRethrow err = do
    -- Wait until all workers have finished
    PoolState{workersActive} <- get
    let allDone = workersActive <= 0

    if allDone
      then lift $ liftM $ Control.Exception.throwIO err
      else do
        -- Read next event and continue waiting
        event <- lift $ liftM $ readChan events
        case event of
          WorkerFinished stmResult -> do
            result <- lift $ restoreM stmResult
            when collectResults $ addResult result
            modifyWorkersActive pred
          WorkerFinishedWithError _ ->
            -- TODO: How to handle multiple errors?
            modifyWorkersActive pred
          AddWork _ -> pure ()
        waitAndRethrow err

wrapWorker ::
  forall m a b.
  (MonadBaseControl IO m) =>
  Chan (Event a b m) ->
  (a -> m b) ->
  a ->
  m ()
wrapWorker events f a = do
  liftBaseWith $ \runInIO ->
    Control.Exception.catch
      ( do
          stm <- runInIO (f a)
          writeChan events (WorkerFinished stm)
      )
      ( \err -> do
          writeChan events (WorkerFinishedWithError err)
      )

addWork :: (MonadBaseControl IO m) => Pool a b m -> a -> m ()
addWork Pool{events} a = liftM $ writeChan events (AddWork a)
