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
  , SpotBidDatum(..)
  , ClaimBidDatum(..)
  , AcceptedBidDatum(..)
  , PaymentDatum(..)

    -- * On-chain Redeemers
  , MarketRedeemer(..)
  , MarketObserverRedeemer(..)
  , BeaconsRedeemer(..)

    -- * Contracts
  , proxyScript
  , proxyValidatorHash
  , aftermarketScript
  , aftermarketScriptHash
  , aftermarketObserverScript
  , aftermarketObserverScriptHash
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
  , NewSpotBidInfo(..)
  , unsafeCreateSpotBidDatum
  , NewClaimBidInfo(..)
  , unsafeCreateClaimBidDatum
  , NewAcceptedBidInfo(..)
  , unsafeCreateAcceptedBidDatum
  , createAcceptedBidDatumFromClaimBid

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
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
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
           , "aftermarket_observer_hash" .= aftermarketObserverHash
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
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
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
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "nft_names" .= nftNames
           , "starting_price" .= startingPrice
           ]

-- | The datum for a SpotBid UTxO for NFTs. These can be immediately claimed by the seller as long
-- as they send the NFTs to the required address.
data SpotBidDatum = SpotBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
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

PlutusTx.makeIsDataIndexed ''SpotBidDatum [('SpotBidDatum,2)]
makeFieldLabelsNoPrefix ''SpotBidDatum

instance ToJSON SpotBidDatum where
  toJSON SpotBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= show @Text (pretty $ toBidderId bidderCredential)
           , "nft_names" .= nftNames
           , "payment_address" .= paymentAddress
           , "bid_deposit" .= bidDeposit
           , "bid" .= bid
           ]

-- | The datum for a ClaimBid UTxO for NFTs. These must be evolved into an AcceptedBid UTxO so that
-- the bidder can come claim the NFTs. This step is required for certain compositions where the
-- buyer needs to update UTxOs for the primary market in the same transaction.
data ClaimBidDatum = ClaimBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. 
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue. If the ClaimBid is accepted and the bidder walks
  -- away, this deposit will be taken by the seller. The bidder can increase the deposit to make
  -- their ClaimBid more enticing for sellers.
  , bidDeposit :: Integer
  -- | The actual bid. This field tells the seller how much you are promising to pay for the NFTs.
  , bid :: Prices
  -- | The time this bid expires.
  , bidExpiration :: Maybe POSIXTime
  -- | The time, after which, the seller can reclaim the NFTs + the bidder's deposit.
  , claimExpiration :: POSIXTime
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''ClaimBidDatum [('ClaimBidDatum,3)]
makeFieldLabelsNoPrefix ''ClaimBidDatum

instance ToJSON ClaimBidDatum where
  toJSON ClaimBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= show @Text (pretty $ toBidderId bidderCredential)
           , "nft_names" .= nftNames
           , "bid_deposit" .= bidDeposit
           , "bid" .= bid
           , "bid_expiration" .= bidExpiration
           , "claim_expiration" .= claimExpiration
           ]

-- | The datum for an AcceptedBid UTxO for NFTs. It contains the required NFTs and is waiting to be
-- claimed by the bidder. The bidder must pay the seller the required bid amount + the seller's
-- deposit to actually claim the NFTs. If the bidder does not claim them, the the seller can
-- re-claim the NFTs after the claim expiration has passed. The seller will also claim the bidder's
-- deposit in this context as compensation.
data AcceptedBidDatum = AcceptedBidDatum
  -- | The policy id for the beacon script.
  { beaconId :: BeaconId
  -- | The hash of the aftermarket observer script.
  , aftermarketObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. Only this credential can claim the
  -- NFTs.
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue. If the bidder walks away, this deposit will 
  -- be taken by the seller. 
  , bidDeposit :: Integer
  -- | The amount the seller paid for the minUTxOValue, over what the bidder paid. This will be
  -- returned to the seller.
  , sellerDeposit :: Integer
  -- | The actual bid. This field tells the seller how much you are promising to pay for the NFTs.
  , bid :: Prices
  -- | The address where the bid payment must go upon claiming the NFTs.
  , paymentAddress :: Address
  -- | The time, after which, the seller can reclaim the NFTs + the bidder's deposit.
  , claimExpiration :: POSIXTime
  } deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''AcceptedBidDatum [('AcceptedBidDatum,4)]
makeFieldLabelsNoPrefix ''AcceptedBidDatum

instance ToJSON AcceptedBidDatum where
  toJSON AcceptedBidDatum{..} =
    object [ "beacon_id" .= beaconId
           , "aftermarket_observer_hash" .= aftermarketObserverHash
           , "nft_policy_id" .= nftPolicyId
           , "bidder_credential" .= show @Text (pretty $ toBidderId bidderCredential)
           , "nft_names" .= nftNames
           , "bid_deposit" .= bidDeposit
           , "seller_deposit" .= sellerDeposit
           , "bid" .= bid
           , "payment_address" .= paymentAddress
           , "claim_expiration" .= claimExpiration
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
  -- | Accept a SpotBid UTxO, and close the associated Auction UTxO.
  | AcceptSpotBid
  -- | Accept a ClaimBid UTxO, and close the associated Auction UTxO. This will create an
  -- AcceptedBid UTxO with the NFTs for the bidder to come claim. The payment address is the address
  -- the seller would like the payment sent to.
  | AcceptClaimBid { sellerDeposit :: Integer, paymentAddress :: Address }
  -- | Claim an AcceptedBid UTxO that belongs to you (ie, you have the bidder credential).
  | ClaimAcceptedBid
  -- | Reclaim the NFTs from an AcceptedBid UTxO after the bidder has failed to claim them. Take the
  -- bidder's deposit as compensation.
  | UnlockUnclaimedAcceptedBid
  deriving (Generic,Show)

makeFieldLabelsNoPrefix ''MarketRedeemer

data MarketObserverRedeemer
  -- | Observe a market payment/acceptance transaction. 
  = ObserveAftermarket { beaconId :: BeaconId }
  -- | Register the script.
  | RegisterAftermarketObserverScript
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
PlutusTx.unstableMakeIsData ''MarketObserverRedeemer
PlutusTx.unstableMakeIsData ''BeaconsRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
aftermarketScript :: SerialisedScript
aftermarketScript = parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.aftermarket_script"

aftermarketScriptHash :: PV2.ScriptHash
aftermarketScriptHash = scriptHash aftermarketScript

aftermarketObserverScript :: SerialisedScript
aftermarketObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.aftermarket_observer_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData aftermarketScriptHash
    ]

aftermarketObserverScriptHash :: PV2.ScriptHash
aftermarketObserverScriptHash = scriptHash aftermarketObserverScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_aftermarket.beacon_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData aftermarketScriptHash
    , PV2.toData aftermarketObserverScriptHash
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
  , aftermarketObserverHash = aftermarketObserverScriptHash
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
  , aftermarketObserverHash = aftermarketObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , startingPrice = Prices startingPrice
  }

-- | Requried information for creating a new SpotBidDatum.
data NewSpotBidInfo = NewSpotBidInfo
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

-- | Convert the spot bid info to the SpotBidDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateSpotBidDatum :: NewSpotBidInfo -> SpotBidDatum
unsafeCreateSpotBidDatum NewSpotBidInfo{..} = SpotBidDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , aftermarketObserverHash = aftermarketObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , paymentAddress = paymentAddress
  , bidDeposit = bidDeposit
  , bidderCredential = bidderCredential
  , bid = Prices bid
  }

-- | Requried information for creating a new ClaimBidDatum.
data NewClaimBidInfo = NewClaimBidInfo
  -- | The policy id for the NFTs being offered. It is also the name for the PolicyBeacon.
  { nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. It is the hash of their staking 
  -- credential.
  , bidderCredential :: Credential
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue.
  , bidDeposit :: Integer
  -- | The actual bid.
  , bid :: [(Asset,Integer)]
  -- | The time this bid expires.
  , bidExpiration :: Maybe POSIXTime
  -- | The time, after which, the seller can reclaim the NFTs.
  , claimExpiration :: POSIXTime
  } deriving (Show)

-- | Convert the claim bid info to the ClaimBidDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateClaimBidDatum :: NewClaimBidInfo -> ClaimBidDatum
unsafeCreateClaimBidDatum NewClaimBidInfo{..} = ClaimBidDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , aftermarketObserverHash = aftermarketObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , bidDeposit = bidDeposit
  , bidderCredential = bidderCredential
  , bid = Prices bid
  , bidExpiration = bidExpiration
  , claimExpiration = claimExpiration
  }

-- | Requried information for creating a new AcceptedBidDatum.
data NewAcceptedBidInfo = NewAcceptedBidInfo
  -- | The policy id for the NFTs being offered. It is also the name for the PolicyBeacon.
  { nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. It is the hash of their staking 
  -- credential.
  , bidderCredential :: Credential
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The amount of ada used for the minUTxOValue from the bidder.
  , bidDeposit :: Integer
  -- | The amount of ada used for the minUTxOValue from the seller.
  , sellerDeposit :: Integer
  -- | The actual bid.
  , bid :: [(Asset,Integer)]
  -- | The time when the seller can reclaim the NFTs.
  , claimExpiration :: POSIXTime
  -- | The payment address where the seller would like the payment to go.
  , paymentAddress :: Address
  } deriving (Show)

-- | Convert the accepted bid info to the AcceptedBidDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateAcceptedBidDatum :: NewAcceptedBidInfo -> AcceptedBidDatum
unsafeCreateAcceptedBidDatum NewAcceptedBidInfo{..} = AcceptedBidDatum
  { beaconId = BeaconId beaconCurrencySymbol
  , aftermarketObserverHash = aftermarketObserverScriptHash
  , nftPolicyId = nftPolicyId
  , nftNames = nftNames
  , bidDeposit = bidDeposit
  , sellerDeposit = sellerDeposit
  , bidderCredential = bidderCredential
  , bid = Prices bid
  , claimExpiration = claimExpiration
  , paymentAddress = paymentAddress
  }

createAcceptedBidDatumFromClaimBid 
  :: Integer 
  -> Address 
  -> ClaimBidDatum 
  -> AcceptedBidDatum
createAcceptedBidDatumFromClaimBid sellerDeposit sellerAddress ClaimBidDatum{..} = 
  AcceptedBidDatum
    { beaconId = beaconId
    , aftermarketObserverHash = aftermarketObserverHash
    , nftPolicyId = nftPolicyId
    , nftNames = nftNames
    , bidDeposit = bidDeposit
    , sellerDeposit = sellerDeposit
    , bidderCredential = bidderCredential
    , bid = bid
    , claimExpiration = claimExpiration
    , paymentAddress = sellerAddress
    }
