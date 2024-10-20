module Test.SpotBidUTxOs.UpdateSpotBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotBidUTxOs.UpdateSpotBidUTxOs.Regressions qualified as Regressions
import Test.SpotBidUTxOs.UpdateSpotBidUTxOs.Failures qualified as Failures
import Test.SpotBidUTxOs.UpdateSpotBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating SpotBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
