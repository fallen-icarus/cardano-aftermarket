module Test.AcceptedBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AcceptedBidUTxOs.CreateAcceptedBidUTxOs qualified as CreateAcceptedBidUTxOs
import Test.AcceptedBidUTxOs.ClaimAcceptedBidUTxOs qualified as ClaimAcceptedBidUTxOs
import Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs qualified as UnlockAcceptedBidUTxOs

tests :: TestTree
tests = testGroup "AcceptedBid UTxOs"
  [ CreateAcceptedBidUTxOs.tests
  , ClaimAcceptedBidUTxOs.tests
  , UnlockAcceptedBidUTxOs.tests
  ]
