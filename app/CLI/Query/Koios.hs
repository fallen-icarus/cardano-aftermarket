{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Query.Koios
  ( queryPersonalAddress
  , submitTx
  , evaluateTx
  , getParams
  , querySpots
  , queryAuctions
  , queryBids
  ) where

import Relude
import Servant.API
import Data.Aeson
import Servant.Client
import qualified Data.Text as T

import CardanoAftermarket

import CLI.Data.Bech32Address
import CLI.Data.MarketUTxO
import CLI.Data.PersonalUTxO
import CLI.Data.TxCBOR

-------------------------------------------------
-- Post Types
-------------------------------------------------
instance ToHttpApiData CurrencySymbol where
  toQueryParam = show

instance ToHttpApiData TokenName where
  toQueryParam = T.pack . showTokenName

-- | A newtype for submitting a list of payment addresses with the "_extended" flag.
newtype ExtendedPaymentAddresses = ExtendedPaymentAddresses [PaymentAddress] 
  deriving (Show)

instance ToJSON ExtendedPaymentAddresses where
  toJSON (ExtendedPaymentAddresses as) = 
    object [ "_addresses" .= map unPaymentAddress as 
           , "_extended" .= True
           ]

newtype TargetAsset = TargetAsset (CurrencySymbol,TokenName)

instance ToJSON TargetAsset where
  toJSON (TargetAsset (currSym,tokName)) = object 
    [ "_asset_list" .= [[T.pack $ show currSym, T.pack $ showTokenName tokName]]
    , "_extended" .= True
    ]

newtype ExtendedUTxOList = ExtendedUTxOList { unUTxOList :: [TxOutRef] } deriving (Show)

instance ToJSON ExtendedUTxOList where
  toJSON (ExtendedUTxOList as) = 
    object [ "_utxo_refs" .= map (\(TxOutRef hash ix) -> T.pack $ show hash <> "#" <> show ix) as
           , "_extended" .= True
           ]

newtype SubmitTxCBOR = SubmitTxCBOR TxCBOR

instance ToJSON SubmitTxCBOR where
  toJSON (SubmitTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("submitTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

newtype EvaluateTxCBOR = EvaluateTxCBOR TxCBOR

instance ToJSON EvaluateTxCBOR where
  toJSON (EvaluateTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("evaluateTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
type KoiosApi
  =     "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [PersonalUTxO]

  :<|>  ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

  :<|>  "cli_protocol_params"
     :> Get '[JSON] Value

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [MarketUTxO]

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] TargetAsset
     :> Post '[JSON] [MarketUTxO]

personalAddressUTxOsApi 
  :<|> submitTxApi
  :<|> evaluateTxApi
  :<|> paramsApi
  :<|> sellerAddresssUTxOsApi
  :<|> assetUTxOsApi 
  = client (Proxy :: Proxy KoiosApi)

-------------------------------------------------
-- High-Level API
-------------------------------------------------
queryPersonalAddress :: PaymentAddress -> ClientM [PersonalUTxO]
queryPersonalAddress addr =
    sortOn (\PersonalUTxO{utxoRef} -> utxoRef) <$>
      personalAddressUTxOsApi select "eq.false" Nothing (ExtendedPaymentAddresses [addr])
  where
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "datum_hash"
        , "asset_list"
        , "reference_script"
        ]

submitTx :: TxCBOR -> ClientM Value
submitTx = submitTxApi . SubmitTxCBOR

evaluateTx :: TxCBOR -> ClientM Value
evaluateTx = evaluateTxApi . EvaluateTxCBOR

getParams :: ClientM Value
getParams = paramsApi

querySpots :: CurrencySymbol -> Maybe PaymentAddress -> ClientM [MarketUTxO]
querySpots nftPolicy  mSellerAddr = do
  let spotBeacon = (beaconCurrencySymbol,"Spot")
      nftBeacon = (beaconCurrencySymbol, unPolicyBeacon $ toPolicyBeacon nftPolicy)
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mSellerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam [nftBeacon]
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset spotBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam [spotBeacon,nftBeacon]
      sellerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

queryAuctions :: CurrencySymbol -> Maybe PaymentAddress -> ClientM [MarketUTxO]
queryAuctions nftPolicy  mSellerAddr = do
  let auctionBeacon = (beaconCurrencySymbol,"Auction")
      nftBeacon = (beaconCurrencySymbol, unPolicyBeacon $ toPolicyBeacon nftPolicy)
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mSellerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam [nftBeacon]
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset auctionBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam [auctionBeacon,nftBeacon]
      sellerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

queryBids :: CurrencySymbol -> Maybe PaymentAddress -> Maybe Credential -> ClientM [MarketUTxO]
queryBids nftPolicy  mSellerAddr mBidderCred = do
  let bidBeacon = (beaconCurrencySymbol,"Bid")
      nftBeacon = (beaconCurrencySymbol, unPolicyBeacon $ toPolicyBeacon nftPolicy)
      bidderId = ((beaconCurrencySymbol,) . unBidderId . toBidderId) <$> mBidderCred
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mSellerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [Just nftBeacon, bidderId]
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset bidBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [Just bidBeacon,Just nftBeacon, bidderId]
      sellerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
assetToQueryParam :: [(CurrencySymbol,TokenName)] -> Text
assetToQueryParam assets = "cs.[" <> T.intercalate "," (go assets) <> "]"
  where
    go [] = []
    go ((currSym,tokName):xs) = 
      let policyId = T.pack $ show currSym
          assetName = T.pack $ showTokenName tokName
       in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs
