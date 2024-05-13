module Test.AuctionUTxOs.CreateAuctionUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AuctionUTxOs.CreateAuctionUTxOs.Regressions qualified as Regressions
import Test.AuctionUTxOs.CreateAuctionUTxOs.Failures qualified as Failures
import Test.AuctionUTxOs.CreateAuctionUTxOs.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Auction UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
