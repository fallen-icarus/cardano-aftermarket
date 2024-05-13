module Test.BidUTxOs.CloseBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.BidUTxOs.CloseBidUTxOs.Regressions qualified as Regressions
import Test.BidUTxOs.CloseBidUTxOs.Failures qualified as Failures
import Test.BidUTxOs.CloseBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Bid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
