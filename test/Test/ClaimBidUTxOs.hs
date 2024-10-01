module Test.ClaimBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ClaimBidUTxOs.CreateClaimBidUTxOs qualified as Create
import Test.ClaimBidUTxOs.CloseClaimBidUTxOs qualified as Close
import Test.ClaimBidUTxOs.UpdateClaimBidUTxOs qualified as Update
import Test.ClaimBidUTxOs.AcceptClaimBidUTxOs qualified as Accept

tests :: TestTree
tests = testGroup "ClaimBid UTxOs"
  [ Create.tests
  , Close.tests
  , Update.tests
  , Accept.tests
  ]
