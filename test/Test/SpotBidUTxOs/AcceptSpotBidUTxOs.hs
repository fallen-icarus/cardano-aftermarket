module Test.SpotBidUTxOs.AcceptSpotBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotBidUTxOs.AcceptSpotBidUTxOs.Regressions qualified as Regressions
import Test.SpotBidUTxOs.AcceptSpotBidUTxOs.Failures qualified as Failures
import Test.SpotBidUTxOs.AcceptSpotBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Accepting SpotBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
