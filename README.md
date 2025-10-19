# Workhorse

[![BSD-2-Clause license](https://img.shields.io/badge/license-BSD--2--Clause-blue.svg)](LICENSE)

Simple queue-based worker pool where jobs can add more jobs.

## Overview
Workhorse is a Haskell library that provides a simple way to manage a pool of worker threads that can process tasks concurrently. It allows tasks to dynamically add more tasks to the pool, making it suitable for workloads with unpredictable or recursive task generation.

## Quick start
### Basic usage (IO)
```haskell
import Control.Concurrent.Workhorse
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let tasks = [1, 2, 3] :: [Int]

  -- Process tasks concurrently and collect results
  results <- doConcurrently worker tasks
  print results
 where
  worker _ n = do
    threadDelay (n * 1000000)  -- Sleep for n seconds
    pure (n * 10)
```

### Discarding results
When you don't need the results, use `doConcurrently_` to avoid memory overhead:

```haskell
import Control.Concurrent.Workhorse

main :: IO ()
main = do
  let tasks = [1..1000000]

  -- Process tasks without accumulating results
  doConcurrently_ worker tasks
 where
  worker _ n = do
    -- Do some work
    putStrLn $ "Processing: " ++ show n
    pure n  -- Result is discarded
```

### Dynamic task generation
Workers can add more tasks to the pool during execution:

```haskell
import Control.Concurrent.Workhorse

main :: IO ()
main = do
  results <- doConcurrently crawler ["https://example.com"]
  print results
 where
  crawler pool url = do
    -- Fetch and process the URL
    links <- fetchLinks url

    -- Add discovered links to the pool
    mapM_ (addWork pool) links

    pure url
```

### Custom configuration
Control the number of workers:

```haskell
import Control.Concurrent.Workhorse
import Data.Default (def)

main :: IO ()
main = do
  let config = def { nWorkers = Just 4 }  -- Use 4 workers
  results <- doConcurrentlyWith config worker tasks
  print results
```

### Lifted Monadic Interface
For use with monad transformer stacks:

```haskell
import Control.Concurrent.Workhorse.Lifted
import Control.Monad.Reader

type App = ReaderT Config IO

runApp :: App a -> IO a
runApp app = runReaderT app initialConfig

main :: IO ()
main = runApp $ do
  results <- doConcurrently worker [1, 2, 3]
  liftIO $ print results
 where
  worker _ n = do
    config <- ask
    liftIO $ putStrLn $ "Processing " ++ show n
    pure (n * 10)
```

## Testing
Run the test suite:

```bash
cabal test
```
## Contributing
Open a PR, be kind. Format your code with `./format.sh`.
