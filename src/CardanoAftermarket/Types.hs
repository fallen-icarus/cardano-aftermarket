{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoAftermarket.Types
  ( BeaconId(..)
  , PolicyBeacon(..)
  , Asset(..)
  , BidderId(..)
  , Prices(..)
  ) where

import Data.Bifunctor (bimap)
import Data.Aeson
import Data.Text qualified as T
import Prettyprinter
import Optics.TH

import qualified PlutusLedgerApi.V2 as PV2

-------------------------------------------------
-- Helpers
-------------------------------------------------
unsafeFromData :: (PV2.UnsafeFromData a) => PV2.Data -> a
unsafeFromData = PV2.unsafeFromBuiltinData . PV2.dataToBuiltinData

-------------------------------------------------
-- BeaconId
-------------------------------------------------
-- | A wrapper around the policy id for the beacon script.
newtype BeaconId = BeaconId { unBeaconId :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON BeaconId where
  toJSON (BeaconId currSym) = toJSON $ T.pack $ show currSym

makeFieldLabelsNoPrefix ''BeaconId

-------------------------------------------------
-- PolicyBeacon
-------------------------------------------------
-- | A wrapper around the policy id for the nfts being sold.
newtype PolicyBeacon = PolicyBeacon { unPolicyBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON PolicyBeacon where
  toJSON (PolicyBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''PolicyBeacon

-------------------------------------------------
-- Asset
-------------------------------------------------
-- | A wrapper around the asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Asset = Asset { unAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq,Ord)

instance PV2.ToData Asset where
  toBuiltinData (Asset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData Asset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap Asset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Asset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    Asset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to Asset"

instance ToJSON Asset where
  toJSON (Asset (currSym,PV2.TokenName tokName)) =
    object [ "policy_id" .= T.pack (show currSym)
           , "asset_name" .= T.pack (show $ PV2.PubKeyHash tokName)
           ]

instance Pretty Asset where
  pretty (Asset (currSym,PV2.TokenName tokName)) = 
    if currSym == "" 
    then "lovelace"
    else pretty $ T.pack (show currSym) <> "." <> T.pack (show $ PV2.PubKeyHash tokName)

makeFieldLabelsNoPrefix ''Asset

-------------------------------------------------
-- BidderId
-------------------------------------------------
-- | A wrapper around the token name for a bidder's credential.
newtype BidderId = BidderId { unBidderId :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON BidderId where
  toJSON (BidderId (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty BidderId where
  pretty (BidderId (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

makeFieldLabelsNoPrefix ''BidderId

-------------------------------------------------
-- Prices
-------------------------------------------------
-- | A wrapper around a list of prices. It uses a custom data encoding since Aiken uses a 
-- different encoding for it.
newtype Prices = Prices { unPrices :: [(Asset,Integer)] }
  deriving (Show,Eq)

instance PV2.ToData Prices where
  toBuiltinData (Prices xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData) xs

instance PV2.FromData Prices where
  fromBuiltinData (PV2.BuiltinData (PV2.Map xs)) = 
    fmap Prices $ sequence $ 
        flip map xs $ \(x,y) -> (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Prices where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map xs)) = 
    Prices $ map (bimap unsafeFromData unsafeFromData) xs
  unsafeFromBuiltinData _ = error "Could not convert Data to Prices"

instance ToJSON Prices where
  toJSON (Prices xs) = object [ "prices" .= map toJSON xs ]

makeFieldLabelsNoPrefix ''Prices

