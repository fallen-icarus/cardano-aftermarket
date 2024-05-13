module Test.SpotUTxOs.UpdateSpotUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotUTxOs.UpdateSpotUTxOs.Regressions qualified as Regressions
import Test.SpotUTxOs.UpdateSpotUTxOs.Failures qualified as Failures
import Test.SpotUTxOs.UpdateSpotUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Spot UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
