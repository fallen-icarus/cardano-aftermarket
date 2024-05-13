module Test.BidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.BidUTxOs.CreateBidUTxOs qualified as CreateBidUTxOs
import Test.BidUTxOs.CloseBidUTxOs qualified as CloseBidUTxOs
import Test.BidUTxOs.UpdateBidUTxOs qualified as UpdateBidUTxOs
import Test.BidUTxOs.AcceptBidUTxOs qualified as AcceptBidUTxOs

tests :: TestTree
tests = testGroup "Bid UTxOs"
  [ CreateBidUTxOs.tests
  , CloseBidUTxOs.tests
  , UpdateBidUTxOs.tests
  , AcceptBidUTxOs.tests
  ]
