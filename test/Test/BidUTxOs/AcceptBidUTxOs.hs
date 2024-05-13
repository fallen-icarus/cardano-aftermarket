module Test.BidUTxOs.AcceptBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.BidUTxOs.AcceptBidUTxOs.Regressions qualified as Regressions
import Test.BidUTxOs.AcceptBidUTxOs.Failures qualified as Failures
import Test.BidUTxOs.AcceptBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Accepting Bid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
