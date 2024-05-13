module Main where

import Test.Tasty

import Test.SpotUTxOs qualified as Spot
import Test.BidUTxOs qualified as Bid
import Test.AuctionUTxOs qualified as Auction
import Test.Misc qualified as Misc
import Test.Beacons qualified as Beacons

main :: IO ()
main = defaultMain $ testGroup "Cardano-Aftermarket"
  [ Spot.tests
  , Bid.tests
  , Auction.tests
  , Misc.tests
  , Beacons.tests
  ]
