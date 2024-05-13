module Test.SpotUTxOs.CloseSpotUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotUTxOs.CloseSpotUTxOs.Regressions qualified as Regressions
import Test.SpotUTxOs.CloseSpotUTxOs.Failures qualified as Failures
import Test.SpotUTxOs.CloseSpotUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Spot UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
