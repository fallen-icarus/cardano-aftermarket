module Test.BidUTxOs.UpdateBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.BidUTxOs.UpdateBidUTxOs.Regressions qualified as Regressions
import Test.BidUTxOs.UpdateBidUTxOs.Failures qualified as Failures
import Test.BidUTxOs.UpdateBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Bid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
