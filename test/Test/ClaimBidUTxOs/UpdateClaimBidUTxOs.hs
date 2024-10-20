module Test.ClaimBidUTxOs.UpdateClaimBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ClaimBidUTxOs.UpdateClaimBidUTxOs.Regressions qualified as Regressions
import Test.ClaimBidUTxOs.UpdateClaimBidUTxOs.Failures qualified as Failures
import Test.ClaimBidUTxOs.UpdateClaimBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating ClaimBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
