module Test.SpotBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.SpotBidUTxOs.CreateSpotBidUTxOs qualified as Create
import Test.SpotBidUTxOs.CloseSpotBidUTxOs qualified as Close
import Test.SpotBidUTxOs.UpdateSpotBidUTxOs qualified as Update
import Test.SpotBidUTxOs.AcceptSpotBidUTxOs qualified as Accept

tests :: TestTree
tests = testGroup "SpotBid UTxOs"
  [ Create.tests
  , Close.tests
  , Update.tests
  , Accept.tests
  ]
