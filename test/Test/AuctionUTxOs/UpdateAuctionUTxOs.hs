module Test.AuctionUTxOs.UpdateAuctionUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AuctionUTxOs.UpdateAuctionUTxOs.Regressions qualified as Regressions
import Test.AuctionUTxOs.UpdateAuctionUTxOs.Failures qualified as Failures
import Test.AuctionUTxOs.UpdateAuctionUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Auction UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
