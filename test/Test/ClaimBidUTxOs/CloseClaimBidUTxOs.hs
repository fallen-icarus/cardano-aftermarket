module Test.ClaimBidUTxOs.CloseClaimBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ClaimBidUTxOs.CloseClaimBidUTxOs.Regressions qualified as Regressions
import Test.ClaimBidUTxOs.CloseClaimBidUTxOs.Failures qualified as Failures
import Test.ClaimBidUTxOs.CloseClaimBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing ClaimBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
