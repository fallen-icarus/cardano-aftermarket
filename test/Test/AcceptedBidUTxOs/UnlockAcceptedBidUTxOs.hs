module Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs.Regressions qualified as Regressions
import Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs.Failures qualified as Failures
import Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Unlocking AcceptedBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
