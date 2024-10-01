module Test.ClaimBidUTxOs.AcceptClaimBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ClaimBidUTxOs.AcceptClaimBidUTxOs.Regressions qualified as Regressions
import Test.ClaimBidUTxOs.AcceptClaimBidUTxOs.Failures qualified as Failures
import Test.ClaimBidUTxOs.AcceptClaimBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Accepting ClaimBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
