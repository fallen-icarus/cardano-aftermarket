module Test.SpotUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotUTxOs.CreateSpotUTxOs qualified as CreateSpotUTxOs
import Test.SpotUTxOs.CloseSpotUTxOs qualified as CloseSpotUTxOs
import Test.SpotUTxOs.UpdateSpotUTxOs qualified as UpdateSpotUTxOs
import Test.SpotUTxOs.PurchaseSpotUTxOs qualified as PurchaseSpotUTxOs

tests :: TestTree
tests = testGroup "Spot UTxOs"
  [ CreateSpotUTxOs.tests
  , CloseSpotUTxOs.tests
  , UpdateSpotUTxOs.tests
  , PurchaseSpotUTxOs.tests
  ]
