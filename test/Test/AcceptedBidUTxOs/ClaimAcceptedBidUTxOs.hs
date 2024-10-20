module Test.AcceptedBidUTxOs.ClaimAcceptedBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AcceptedBidUTxOs.ClaimAcceptedBidUTxOs.Regressions qualified as Regressions
import Test.AcceptedBidUTxOs.ClaimAcceptedBidUTxOs.Failures qualified as Failures
import Test.AcceptedBidUTxOs.ClaimAcceptedBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Claiming AcceptedBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
