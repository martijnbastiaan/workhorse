module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Workhorse
import Data.List (sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Workhorse Tests"
    [ testGroup
        "Concurrent Execution"
        [ testCase "sleeps execute concurrently" testConcurrentSleeps
        , testCase "sleeps with results" testConcurrentSleepsWithResults
        , testCase "sleeps without results" testConcurrentSleepsWithoutResults
        ]
    ]

{- | Test that sleep 1, sleep 2, and sleep 3 execute concurrently
and complete in less than 4 seconds (instead of 6 if sequential)
-}
testConcurrentSleeps :: Assertion
testConcurrentSleeps = do
  start <- getCurrentTime
  let
    sleeps = [1, 2, 3] :: [Int]
    worker _ seconds = do
      threadDelay (seconds * 1000000) -- Convert to microseconds
      pure seconds

  results <- doConcurrently worker sleeps
  end <- getCurrentTime

  let duration = realToFrac (diffUTCTime end start) :: Double

  -- Should complete in ~3 seconds (longest sleep) plus some overhead
  -- We allow up to 4 seconds to account for scheduling and overhead
  assertBool
    ("Expected execution in less than 4 seconds, took " ++ show duration ++ " seconds")
    (duration < 4.0)

  -- Verify we got results (order may vary due to concurrency)
  assertEqual "Should have 3 results" 3 (length results)
  assertEqual "Results should contain all sleep durations" [1, 2, 3] (sort results)

-- | Test doConcurrently collects and returns results
testConcurrentSleepsWithResults :: Assertion
testConcurrentSleepsWithResults = do
  let
    sleeps = [1, 2, 3] :: [Int]
    worker _ seconds = do
      threadDelay (seconds * 1000000)
      pure (seconds * 10) -- Return transformed value
  results <- doConcurrently worker sleeps

  assertEqual "Should have 3 results" 3 (length results)
  -- Results contain transformed values
  assertBool "Results should contain all transformed values" $
    all (`elem` results) [10, 20, 30]

-- | Test doConcurrently_ discards results
testConcurrentSleepsWithoutResults :: Assertion
testConcurrentSleepsWithoutResults = do
  let
    sleeps = [1, 2, 3] :: [Int]
    worker _ seconds = do
      threadDelay (seconds * 1000000)
      pure (seconds * 10)

  -- This should not accumulate results
  doConcurrently_ worker sleeps

  -- If we get here without running out of memory or hanging, the test passes
  assertBool "doConcurrently_ should complete successfully" True
