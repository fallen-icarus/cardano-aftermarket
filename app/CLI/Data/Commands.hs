{-# LANGUAGE StrictData #-}

module CLI.Data.Commands where

import Relude

import CardanoAftermarket

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.Output

data Command
  = ExportScript Script FilePath
  | CreateDatum NewDatum FilePath
  | CreateRedeemer NewRedeemer FilePath
  | BeaconName BeaconName Output
  | ConvertTime ConvertTime Network
  | Query Query
  | SubmitTx Network ApiService FilePath
  | EvaluateTx Network ApiService FilePath

data Script 
  = BeaconScript
  | MarketObserverScript
  | MarketScript
  | ProxyScript

data NewDatum
  = NewSpotDatum NewSpotInfo
  | NewSpotBidDatum NewSpotBidInfo
  | NewClaimBidDatum NewClaimBidInfo
  | NewAuctionDatum NewAuctionInfo
  | NewPaymentDatum TxOutRef
  | NewAcceptedBidDatumManual NewAcceptedBidInfo
  | NewAcceptedBidDatumAuto Network ApiService TxOutRef Integer Address

data NewRedeemer
  = NewBeaconRedeemer BeaconsRedeemer
  | NewMarketRedeemer MarketRedeemer
  | NewMarketObserverRedeemer MarketObserverRedeemer

data BeaconName
  = BeaconPolicyId
  | SpotBeaconName
  | AuctionBeaconName
  | BidBeaconName
  | BidderIdName Credential
  | PolicyBeaconName CurrencySymbol

data ConvertTime
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data Query
  -- | Query a user's personal address.
  = QueryPersonal Network ApiService PaymentAddress Format Output
  -- | Query the current protocol parameters.
  | QueryParameters Network ApiService Output
  -- | Query all open Spot UTxOs for a given policy id. You can optionally filter by seller address.
  | QuerySpots Network ApiService CurrencySymbol (Maybe Credential) Format Output
  -- | Query all open Auction UTxOs for a given policy id. You can optionally filter by seller address.
  | QueryAuctions Network ApiService CurrencySymbol (Maybe Credential) Format Output
  -- | Query all open Bid UTxOs for a given policy id. You can optionally filter by seller address
  -- and/or BidderId.
  | QueryBids Network ApiService CurrencySymbol (Maybe Credential) (Maybe Credential) Format Output
