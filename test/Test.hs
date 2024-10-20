module Main where

import Test.Tasty

import Test.SpotUTxOs qualified as Spot
import Test.SpotBidUTxOs qualified as SpotBid
import Test.ClaimBidUTxOs qualified as ClaimBid
import Test.AcceptedBidUTxOs qualified as AcceptedBid
import Test.AuctionUTxOs qualified as Auction
import Test.Misc qualified as Misc
import Test.Beacons qualified as Beacons

main :: IO ()
main = defaultMain $ testGroup "Cardano-Aftermarket"
  [ Spot.tests
  , Auction.tests
  , SpotBid.tests
  , ClaimBid.tests
  , AcceptedBid.tests
  , Misc.tests
  , Beacons.tests
  ]
