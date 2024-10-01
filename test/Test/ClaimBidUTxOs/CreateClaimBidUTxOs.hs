module Test.ClaimBidUTxOs.CreateClaimBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ClaimBidUTxOs.CreateClaimBidUTxOs.Regressions qualified as Regressions
import Test.ClaimBidUTxOs.CreateClaimBidUTxOs.Failures qualified as Failures
import Test.ClaimBidUTxOs.CreateClaimBidUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating ClaimBid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
