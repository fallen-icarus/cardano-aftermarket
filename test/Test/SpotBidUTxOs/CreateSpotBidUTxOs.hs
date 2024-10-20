module Test.SpotBidUTxOs.CreateSpotBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotBidUTxOs.CreateSpotBidUTxOs.Regressions qualified as Regressions
import Test.SpotBidUTxOs.CreateSpotBidUTxOs.Failures qualified as Failures
import Test.SpotBidUTxOs.CreateSpotBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating SpotBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
