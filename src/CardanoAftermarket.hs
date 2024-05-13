{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoAftermarket
  ( -- * On-chain Datums
    SpotDatum(..)
  , AuctionDatum(..)
  , BidDatum(..)
  , PaymentDatum(..)

    -- * On-chain Redeemers
  , MarketRedeemer(..)
  , PaymentObserverRedeemer(..)
  , BeaconsRedeemer(..)

    -- * Contracts
  , proxyScript
  , proxyValidatorHash
  , aftermarketScript
  , aftermarketScriptHash
  , paymentObserverScript
  , paymentObserverScriptHash
  , beaconScript
  , beaconScriptHash
  , beaconCurrencySymbol

    -- * Beacon Names
  , toBidderId
  , toPolicyBeacon

    -- * Creating Datums
  , NewSpotInfo(..)
  , unsafeCreateSpotDatum
  , NewAuctionInfo(..)
  , unsafeCreateAuctionDatum
  , NewBidInfo(..)
  , unsafeCreateBidDatum

    -- * Re-exports
  , module CardanoAftermarket.Types
  , module CardanoAftermarket.Utils
  ) where

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2
import Relude
import Optics.TH
import Prettyprinter (pretty)

import CardanoLoans (proxyScript,proxyValidatorHash)

import CardanoAftermarket.Blueprints
import CardanoAftermarket.Types
import CardanoAftermarket.Utils

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Convert an NFT's policy id to a PolicyBeacon.
toPolicyBeacon :: PV2.CurrencySymbol -> PolicyBeacon
toPolicyBeacon (PV2.CurrencySymbol currSym) = 
  PolicyBeacon $ PV2.TokenName $ PlutusTx.sha2_256 $ unsafeToBuiltinByteString "00" <> currSym

-- | Convert a credential to a BidderId.
toBidderId :: PV2.Credential -> BidderId
toBidderId (PV2.PubKeyCredential (PV2.PubKeyHash pkh)) = 
  BidderId $ TokenName $ unsafeToBuiltinByteString "01" <> pkh
toBidderId (PV2.ScriptCredential (PV2.ScriptHash sh)) = 
  BidderId $ TokenName $ unsafeToBuiltinByteString "01" <> sh

-------------------------------------------------
-- On-Chain Datums
-------------------------------------------------
-- | The datum for a Spot UTxO sale for NFTs.
data SpotDatum = SpotDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being offered.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The address where the proceeds must go upon purchase of the Spot UTxO.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , saleDeposit :: Integer
  -- | The price for this batch, denominated in the specified assets. This is the price for the
  -- entire batch of NFTs. The payment output must contain *all* of the required assets, and their
  -- specified amounts.
  , salePrice :: Prices
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''SpotDatum [('SpotDatum,0)]
makeFieldLabelsNoPrefix ''SpotDatum

instance ToJSON SpotDatum where
  toJSON SpotDatum{..} =
    object [ "beacon_id" .= beaconId
           , "payment_observer_hash" .= paymentObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "nft_names" .= nftNames
           , "payment_address" .= paymentAddress
           , "sale_deposit" .= saleDeposit
           , "sale_price" .= salePrice
           ]

-- | The datum for an Auction UTxO for NFTs.
data AuctionDatum = AuctionDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being auctioned.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The desired starting price. This is only used to broadcast the auctioner's desired value.
  -- Bidders are able to create "counter-bids" with different assets and/or different nft names.
  -- For example, the bidder can make a bid for just one of the NFTs in the batch.
  , startingPrice :: Prices
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''AuctionDatum [('AuctionDatum,1)]
makeFieldLabelsNoPrefix ''AuctionDatum

instance ToJSON AuctionDatum where
  toJSON AuctionDatum{..} =
    object [ "beacon_id" .= beaconId
           , "payment_observer_hash" .= paymentObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "nft_names" .= nftNames
           , "starting_price" .= startingPrice
           ]

-- | The datum for a Bid UTxO for NFTs.
data BidDatum = BidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. 
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The address where the NFTs must go upon accepting the bid.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , bidDeposit :: Integer
  -- | The actual bid.
  , bid :: Prices
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''BidDatum [('BidDatum,2)]
makeFieldLabelsNoPrefix ''BidDatum

instance ToJSON BidDatum where
  toJSON BidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "payment_observer_hash" .= paymentObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= show @Text (pretty $ toBidderId bidderCredential)
           , "nft_names" .= nftNames
           , "payment_address" .= paymentAddress
           , "bid_deposit" .= bidDeposit
           , "bid" .= bid
           ]

-- | The `CurrencySymbol` is always the beacon policy id, and the `TxOutRef` is always
-- the output reference for either the Spot UTxO being purchased or the Bid UTxO being accepted.
newtype PaymentDatum = PaymentDatum (BeaconId,TxOutRef)
  deriving (Generic)

instance PV2.ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,ref)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData ref]

instance PV2.FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,ref])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData ref
  fromBuiltinData _ = Nothing

-------------------------------------------------
-- On-Chain Redeemers
-------------------------------------------------
data MarketRedeemer
  -- | Close or update either a Spot UTxO or an Auction UTxO.
  = CloseOrUpdateSellerUTxO
  -- | Close or update a Bid UTxO.
  | CloseOrUpdateBidderUTxO
  -- | Purchase a Spot UTxO.
  | PurchaseSpot
  -- | Accept a Bid UTxO, and close the associated Auction UTxO.
  | AcceptBid
  deriving (Generic,Show)

makeFieldLabelsNoPrefix ''MarketRedeemer

data PaymentObserverRedeemer
  -- | Observe a market payment transaction. This observes the payment for both spot purchases and
  -- bid acceptances.
  = ObservePayment { beaconId :: BeaconId }
  -- | Register the script.
  | RegisterPaymentObserverScript
  deriving (Generic,Show)

data BeaconsRedeemer
  -- | Create, close, or update some market UTxOs (1 or more). 
  = CreateCloseOrUpdateMarketUTxOs
  -- | Burn any beacons. 
  | BurnBeacons
  -- | Register the script.
  | RegisterBeaconsScript
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.unstableMakeIsData ''PaymentObserverRedeemer
PlutusTx.unstableMakeIsData ''BeaconsRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
aftermarketScript :: SerialisedScript
aftermarketScript = parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.aftermarket_script"

aftermarketScriptHash :: PV2.ScriptHash
aftermarketScriptHash = scriptHash aftermarketScript

paymentObserverScript :: SerialisedScript
paymentObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.payment_observer_script")
    [ PV2.toData aftermarketScriptHash
    ]

paymentObserverScriptHash :: PV2.ScriptHash
paymentObserverScriptHash = scriptHash paymentObserverScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.beacon_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData aftermarketScriptHash
    , PV2.toData paymentObserverScriptHash
    ]

beaconScriptHash :: PV2.ScriptHash
beaconScriptHash = scriptHash beaconScript

beaconCurrencySymbol :: PV2.CurrencySymbol
beaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash beaconScriptHash

-------------------------------------------------
-- Creating Datums
-------------------------------------------------
-- | Requried information for creating a new SpotDatum
data NewSpotInfo = NewSpotInfo
  -- | The policy id for the NFTs being offered. It is also the name for the PolicyBeacon.
  { nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The address where the proceeds must go upon purchase of the Spot UTxO.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , saleDeposit :: Integer
  -- | The price for this batch, denominated in the specified assets. This is the price for the
  -- entire batch of NFTs. The payment output must contain *all* of the required assets.
  , salePrice :: [(Asset,Integer)]
  } deriving (Show)

-- | Convert the spot info to the SpotDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateSpotDatum :: NewSpotInfo -> SpotDatum
unsafeCreateSpotDatum NewSpotInfo{..} = SpotDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , paymentObserverHash = paymentObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , paymentAddress = paymentAddress
  , saleDeposit = saleDeposit
  , salePrice = Prices salePrice
  }

-- | Requried information for creating a new AuctionDatum
data NewAuctionInfo = NewAuctionInfo
  -- | The policy id for the NFTs being offered. It is also the name for the PolicyBeacon.
  { nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The desired starting price. This is only used to broadcast the auctioner's desired value.
  -- Bidders are able to create "counter-bids" with different assets and/or different nft names.
  -- For example, the bidder can make a bid for just one of the NFTs in the batch.
  , startingPrice :: [(Asset,Integer)]
  } deriving (Show)

-- | Convert the auction info to the AuctoinDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateAuctionDatum :: NewAuctionInfo -> AuctionDatum
unsafeCreateAuctionDatum NewAuctionInfo{..} = AuctionDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , paymentObserverHash = paymentObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , startingPrice = Prices startingPrice
  }

-- | Requried information for creating a new BidDatum.
data NewBidInfo = NewBidInfo
  -- | The policy id for the NFTs being offered. It is also the name for the PolicyBeacon.
  { nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. It is the hash of their staking 
  -- credential.
  , bidderCredential :: Credential
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The address where the NFTs must go upon accepting the bid.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , bidDeposit :: Integer
  -- | The actual bid.
  , bid :: [(Asset,Integer)]
  } deriving (Show)

-- | Convert the bid info to the BidDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateBidDatum :: NewBidInfo -> BidDatum
unsafeCreateBidDatum NewBidInfo{..} = BidDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , paymentObserverHash = paymentObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , paymentAddress = paymentAddress
  , bidDeposit = bidDeposit
  , bidderCredential = bidderCredential
  , bid = Prices bid
  }

