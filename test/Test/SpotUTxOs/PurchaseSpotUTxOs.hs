module Test.SpotUTxOs.PurchaseSpotUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotUTxOs.PurchaseSpotUTxOs.Regressions qualified as Regressions
import Test.SpotUTxOs.PurchaseSpotUTxOs.Failures qualified as Failures
import Test.SpotUTxOs.PurchaseSpotUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Purchasing Spot UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
