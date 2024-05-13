module Test.AuctionUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AuctionUTxOs.CreateAuctionUTxOs qualified as CreateAuctionUTxOs
import Test.AuctionUTxOs.CloseAuctionUTxOs qualified as CloseAuctionUTxOs
import Test.AuctionUTxOs.UpdateAuctionUTxOs qualified as UpdateAuctionUTxOs

tests :: TestTree
tests = testGroup "Auction UTxOs"
  [ CreateAuctionUTxOs.tests
  , CloseAuctionUTxOs.tests
  , UpdateAuctionUTxOs.tests
  ]
