module Test.SpotBidUTxOs.CloseSpotBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotBidUTxOs.CloseSpotBidUTxOs.Regressions qualified as Regressions
import Test.SpotBidUTxOs.CloseSpotBidUTxOs.Failures qualified as Failures
import Test.SpotBidUTxOs.CloseSpotBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing SpotBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
