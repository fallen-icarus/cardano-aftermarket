module Test.BidUTxOs.CreateBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.BidUTxOs.CreateBidUTxOs.Regressions qualified as Regressions
import Test.BidUTxOs.CreateBidUTxOs.Failures qualified as Failures
import Test.BidUTxOs.CreateBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Bid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
