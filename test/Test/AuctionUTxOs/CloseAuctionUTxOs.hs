module Test.AuctionUTxOs.CloseAuctionUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AuctionUTxOs.CloseAuctionUTxOs.Regressions qualified as Regressions
import Test.AuctionUTxOs.CloseAuctionUTxOs.Failures qualified as Failures
import Test.AuctionUTxOs.CloseAuctionUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Auction UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
