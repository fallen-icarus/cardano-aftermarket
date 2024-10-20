{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.MarketUTxO where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Asset
import CLI.Data.Bech32Address
import CLI.Data.Network

import CardanoAftermarket

data MarketDatum
  = Spot SpotDatum
  | Auction AuctionDatum
  | SpotBid SpotBidDatum
  | ClaimBid ClaimBidDatum
  | AcceptedBid AcceptedBidDatum
  deriving (Show)

instance ToJSON MarketDatum where
  toJSON (Spot datum) =
    object [ "type" .= ("spot" :: Text)
           , "datum" .= datum
           ]
  toJSON (Auction datum) =
    object [ "type" .= ("auction" :: Text)
           , "datum" .= datum
           ]
  toJSON (SpotBid datum) =
    object [ "type" .= ("spot_bid" :: Text)
           , "datum" .= datum
           ]
  toJSON (ClaimBid datum) =
    object [ "type" .= ("claim_bid" :: Text)
           , "datum" .= datum
           ]
  toJSON (AcceptedBid datum) =
    object [ "type" .= ("accepted_bid" :: Text)
           , "datum" .= datum
           ]

data MarketUTxO = MarketUTxO
  { marketAddress :: PaymentAddress
  , utxoRef :: TxOutRef
  , lovelaces :: Lovelace
  , nativeAssets :: [NativeAsset]
  , marketDatum :: Maybe MarketDatum
  } deriving (Show)

instance FromJSON MarketUTxO where
  parseJSON =
      withObject "MarketUTxO" $ \o ->
        MarketUTxO
          <$> o .: "address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> o .: "asset_list"
          <*> (o .:? "inline_datum" >>= 
                maybe (return Nothing) (\i -> withObject "inlineDatum" (.: "value") i >>= return . parseDatum))
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

      parseDatum :: Value -> Maybe MarketDatum
      parseDatum v = asum
        [ Spot <$> decodeDatum @SpotDatum v
        , Auction <$> decodeDatum @AuctionDatum v
        , SpotBid <$> decodeDatum @SpotBidDatum v
        , ClaimBid <$> decodeDatum @ClaimBidDatum v
        , AcceptedBid <$> decodeDatum @AcceptedBidDatum v
        ]

instance ToJSON MarketUTxO where
  toJSON MarketUTxO{..} =
    object [ "market_address" .= marketAddress
           , "utxo_id" .= (\(TxOutRef hash idx) -> show @Text hash <> "#" <> show idx) utxoRef
           , "native_assets" .= nativeAssets
           , "lovelace" .= unLovelace lovelaces
           , "info" .= marketDatum
           ]

prettyMarketUTxO :: Network -> MarketUTxO -> Doc AnsiStyle
prettyMarketUTxO network MarketUTxO{utxoRef=(TxOutRef hash idx),..} = 
  vsep [ annotate (colorDull Yellow) "utxo_ref:" <+> show hash <> "#" <> show idx
       , indent 4 $ annotate (colorDull Green) "market_address:" <+> pretty marketAddress 
       , indent 4 $ annotate (colorDull Green) "assets:"
       , indent 6 $ pretty lovelaces <+> "lovelace"
       , indent 6 $ align $ vsep $ map pretty nativeAssets
       , indent 4 $ 
           maybe (annotate (colorDull Cyan) "datum:" <+> "none") prettyMarketDatum marketDatum
       , mempty
       ]
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

    interspersePlus :: [Doc AnsiStyle] -> [Doc AnsiStyle]
    interspersePlus [] = []
    interspersePlus [x] = [x]
    interspersePlus (x:xs) = 
      let go [] = []
          go (y:ys) = ("+" <+> y) : go ys
      in x : go xs

    prettyPrice :: (Asset,Integer) -> Doc AnsiStyle
    prettyPrice (Asset (currSym,name), num) =
      if currSym == "" then pretty num <+> "lovelace" else
        pretty num <+> pretty (show currSym <> "." <> showTokenName name)

    prettyCredential :: Credential -> Doc a
    prettyCredential (PubKeyCredential pk) = pretty pk <+> "(pubkey)"
    prettyCredential (ScriptCredential sh) = pretty sh <+> "(script)"

    prettyMarketDatum :: MarketDatum -> Doc AnsiStyle
    prettyMarketDatum (Spot SpotDatum{salePrice = Prices salePrice, ..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Spot"
           , annotate (colorDull Cyan) "nft_policy_id:" <+> pretty (show @Text nftPolicyId)
           , annotate (colorDull Cyan) "sale_deposit:" <+> pretty (Lovelace saleDeposit)
           , annotate (colorDull Cyan) "payment_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ 
                 plutusToBech32 network paymentAddress)
           , annotate (colorDull Cyan) "nfts:"
           , indent 2 $ align $ vsep $ map (pretty . showTokenName) nftNames
           , annotate (colorDull Cyan) "price:"
           , indent 2 $ align $ vsep $ interspersePlus $ map prettyPrice salePrice
           ]
    prettyMarketDatum (SpotBid SpotBidDatum{bid = Prices bid, ..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "SpotBid"
           , annotate (colorDull Cyan) "bidder_credential:" <+> prettyCredential bidderCredential
           , annotate (colorDull Cyan) "nft_policy_id:" <+> pretty (show @Text nftPolicyId)
           , annotate (colorDull Cyan) "bid_deposit:" <+> pretty (Lovelace bidDeposit)
           , annotate (colorDull Cyan) "payment_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ 
                 plutusToBech32 network paymentAddress)
           , annotate (colorDull Cyan) "nfts:"
           , indent 2 $ align $ vsep $ map (pretty . showTokenName) nftNames
           , annotate (colorDull Cyan) "bid:"
           , indent 2 $ align $ vsep $ interspersePlus $ map prettyPrice bid
           ]
    prettyMarketDatum (Auction AuctionDatum{startingPrice = Prices startingPrice, ..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Auction"
           , annotate (colorDull Cyan) "nft_policy_id:" <+> pretty (show @Text nftPolicyId)
           , annotate (colorDull Cyan) "nfts:"
           , indent 2 $ align $ vsep $ map (pretty . showTokenName) nftNames
           , annotate (colorDull Cyan) "starting_price:"
           , indent 2 $ align $ vsep $ interspersePlus $ map prettyPrice startingPrice
           ]
    prettyMarketDatum (ClaimBid ClaimBidDatum{bid = Prices bid, ..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "ClaimBid"
           , annotate (colorDull Cyan) "bidder_credential:" <+> prettyCredential bidderCredential
           , annotate (colorDull Cyan) "nft_policy_id:" <+> pretty (show @Text nftPolicyId)
           , annotate (colorDull Cyan) "bid_deposit:" <+> pretty (Lovelace bidDeposit)
           , annotate (colorDull Cyan) "bid_expiration:" <+> (flip (maybe "none") bidExpiration $ 
               \posix@(POSIXTime time) ->
                  pretty (posixTimeToSlot config posix) <+> tupled [pretty time <+> "milliseconds"])
           , let time = getPOSIXTime claimExpiration in
               annotate (colorDull Cyan) "claim_expiration:" 
                 <+> pretty (posixTimeToSlot config claimExpiration)
                 <+> tupled [pretty time <+> "posix"]
           , annotate (colorDull Cyan) "nfts:"
           , indent 2 $ align $ vsep $ map (pretty . showTokenName) nftNames
           , annotate (colorDull Cyan) "bid:"
           , indent 2 $ align $ vsep $ interspersePlus $ map prettyPrice bid
           ]
    prettyMarketDatum (AcceptedBid AcceptedBidDatum{bid = Prices bid, ..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "AcceptedBid"
           , annotate (colorDull Cyan) "bidder_credential:" <+> prettyCredential bidderCredential
           , annotate (colorDull Cyan) "nft_policy_id:" <+> pretty (show @Text nftPolicyId)
           , annotate (colorDull Cyan) "bid_deposit:" <+> pretty (Lovelace bidDeposit)
           , annotate (colorDull Cyan) "seller_deposit:" <+> pretty (Lovelace sellerDeposit)
           , annotate (colorDull Cyan) "payment_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ 
                 plutusToBech32 network paymentAddress)
           , let time = getPOSIXTime claimExpiration in
               annotate (colorDull Cyan) "claim_expiration:" 
                 <+> pretty (posixTimeToSlot config claimExpiration)
                 <+> tupled [pretty time <+> "posix"]
           , annotate (colorDull Cyan) "nfts:"
           , indent 2 $ align $ vsep $ map (pretty . showTokenName) nftNames
           , annotate (colorDull Cyan) "bid:"
           , indent 2 $ align $ vsep $ interspersePlus $ map prettyPrice bid
           ]
