module Test.SpotUTxOs.CreateSpotUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotUTxOs.CreateSpotUTxOs.Regressions qualified as Regressions
import Test.SpotUTxOs.CreateSpotUTxOs.Failures qualified as Failures
import Test.SpotUTxOs.CreateSpotUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Spot UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
