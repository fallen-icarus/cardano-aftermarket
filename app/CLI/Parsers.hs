module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Relude

import CardanoAftermarket

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Commands
import CLI.Data.Network
import CLI.Data.Output

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "scripts" $
      info parseExportScript $ progDesc "Export a protocol plutus script."
  , command "datums" $
      info parseCreateDatum $ progDesc "Create a datum for the protocol."
  , command "redeemers" $
      info parseCreateRedeemer $ progDesc "Create a redeemer for the protocol."
  , command "beacon-name" $
      info parseBeaconName $ progDesc "Calculate a beacon policy id or asset name."
  , command "query" $
      info parseQuery $ progDesc "Query the blockchain."
  , command "submit" $
      info pSubmitTx $ progDesc "Submit a transaction to the blockchain."
  , command "evaluate-tx" $
      info pEvaluateTx $ progDesc "Estimate script execution units for a transaction."
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = 
    ExportScript
      <$> pScript
      <*> pOutputFile
  where
    pScript :: Parser Script
    pScript = pBeaconScript
          <|> pPaymentScript
          <|> pMarketScript
          <|> pProxyScript

    pBeaconScript :: Parser Script
    pBeaconScript = flag' BeaconScript
      (  long "beacon-script"
      <> help "Export the beacon script."
      )

    pPaymentScript :: Parser Script
    pPaymentScript = flag' PaymentObserverScript
      (  long "payment-script"
      <> help "Export the payment observer script."
      )

    pMarketScript :: Parser Script
    pMarketScript = flag' MarketScript
      (  long "market-script"
      <> help "Export the market spending script."
      )

    pProxyScript :: Parser Script
    pProxyScript = flag' ProxyScript
      (  long "proxy-script"
      <> help "Export the proxy script."
      )

-------------------------------------------------
-- CreateDatum Parser
-------------------------------------------------
parseCreateDatum :: Parser Command
parseCreateDatum = hsubparser $ mconcat
  [ command "spot" $
      info pCreateNewSpotInfo $ progDesc "Create a SpotDatum."
  , command "auction" $
      info pCreateNewAuctionInfo $ progDesc "Create an AuctionDatum."
  , command "bid" $
      info pCreateNewBidInfo $ progDesc "Create a BidDatum."
  , command "payment" $
      info pCreatePaymentDatum $ progDesc "Create a PaymentDatum."
  ]

pCreatePaymentDatum :: Parser Command
pCreatePaymentDatum = 
  CreateDatum 
    <$> (NewPaymentDatum <$> pMarketRef)
    <*> pOutputFile

pCreateNewSpotInfo :: Parser Command
pCreateNewSpotInfo =
    CreateDatum
      <$> pNewSpotInfo
      <*> pOutputFile
  where
    pNewSpotInfo :: Parser NewDatum
    pNewSpotInfo =
      fmap NewSpotDatum $ NewSpotInfo
        <$> pNftPolicyId
        <*> some pNftName
        <*> pPaymentAddress
        <*> pDeposit
        <*> some pPrice

pCreateNewAuctionInfo :: Parser Command
pCreateNewAuctionInfo =
    CreateDatum
      <$> pNewAuctionInfo
      <*> pOutputFile
  where
    pNewAuctionInfo :: Parser NewDatum
    pNewAuctionInfo =
      fmap NewAuctionDatum $ NewAuctionInfo
        <$> pNftPolicyId
        <*> some pNftName
        <*> some pPrice

pCreateNewBidInfo :: Parser Command
pCreateNewBidInfo =
    CreateDatum
      <$> pNewBidInfo
      <*> pOutputFile
  where
    pNewBidInfo :: Parser NewDatum
    pNewBidInfo =
      fmap NewBidDatum $ NewBidInfo
        <$> pNftPolicyId
        <*> pBidderCredential
        <*> some pNftName
        <*> pPaymentAddress
        <*> pDeposit
        <*> some pPrice

-------------------------------------------------
-- CreateRedeemer Parser
-------------------------------------------------
parseCreateRedeemer :: Parser Command
parseCreateRedeemer = hsubparser $ mconcat
    [ command "beacon-script" $
        info pBeaconRedeemer $ progDesc "Create a redeemer for the beacon script."
    , command "market-script" $
        info pMarketRedeemer $ progDesc "Create a redeemer for the aftermarket script."
    , command "payment-script" $
        info pPaymentObserverRedeemer $ progDesc "Create a redeemer for the payment observer script."
    ]

pBeaconRedeemer :: Parser Command
pBeaconRedeemer = hsubparser $ mconcat
    [ command "manage-market-utxos" $
        info pCreateCloseOrUpdateMarketUTxOs $ 
          progDesc "Create the redeemer for creating/updating/closing Market UTxOs."
    , command "burn-all" $
        info pBurnBeacons $ progDesc "Create the redeemer for burning all beacons."
    , command "register" $
        info pRegisterBeaconScript $ progDesc "Create the redeemer for registering the script."
    ]
  where
    pCreateCloseOrUpdateMarketUTxOs :: Parser Command
    pCreateCloseOrUpdateMarketUTxOs = 
      CreateRedeemer
        <$> pure (NewBeaconRedeemer CreateCloseOrUpdateMarketUTxOs)
        <*> pOutputFile

    pBurnBeacons :: Parser Command
    pBurnBeacons = 
      CreateRedeemer 
        <$> pure (NewBeaconRedeemer BurnBeacons)
        <*> pOutputFile

    pRegisterBeaconScript :: Parser Command
    pRegisterBeaconScript =
      CreateRedeemer 
        <$> pure (NewBeaconRedeemer RegisterBeaconsScript)
        <*> pOutputFile
        
pPaymentObserverRedeemer :: Parser Command
pPaymentObserverRedeemer = hsubparser $ mconcat
    [ command "observe-payment" $
        info pObservePayment $ 
          progDesc "Create the redeemer for observing a payment."
    , command "register" $
        info pRegisterPaymentObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObservePayment :: Parser Command
    pObservePayment = 
      CreateRedeemer 
        <$> pure (NewPaymentObserverRedeemer $ ObservePayment $ BeaconId beaconCurrencySymbol)
        <*> pOutputFile

    pRegisterPaymentObserverScript :: Parser Command
    pRegisterPaymentObserverScript =
      CreateRedeemer 
        <$> pure (NewPaymentObserverRedeemer RegisterPaymentObserverScript)
        <*> pOutputFile

pMarketRedeemer :: Parser Command
pMarketRedeemer = hsubparser $ mconcat
    [ command "manage-seller-utxo" $
        info pCloseOrUpdateSellerUTxO $ 
          progDesc "Create the redeemer for updating/closing Spot UTxOs or Auction UTxOs."
    , command "manage-bidder-utxo" $
        info pCloseOrUpdateBidderUTxO $ 
          progDesc "Create the redeemer for updating/closing Bid UTxOs."
    , command "purchase-spot" $
        info pPurchaseSpot $ 
          progDesc "Create the redeemer for purchasing a Spot UTxO."
    , command "accept-bid" $
        info pAcceptBid $ 
          progDesc "Create the redeemer for accepting a Bid UTxO."
    ]
  where
    pCloseOrUpdateSellerUTxO :: Parser Command
    pCloseOrUpdateSellerUTxO = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer CloseOrUpdateSellerUTxO)
        <*> pOutputFile

    pCloseOrUpdateBidderUTxO :: Parser Command
    pCloseOrUpdateBidderUTxO = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer CloseOrUpdateBidderUTxO)
        <*> pOutputFile

    pPurchaseSpot :: Parser Command
    pPurchaseSpot = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer PurchaseSpot)
        <*> pOutputFile

    pAcceptBid :: Parser Command
    pAcceptBid = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer AcceptBid)
        <*> pOutputFile

-------------------------------------------------
-- Beacon Name Parsers
-------------------------------------------------
parseBeaconName :: Parser Command
parseBeaconName = hsubparser $ mconcat
    [ command "policy-id" $
        info pBeaconPolicyId $ progDesc "Calculate the beacon policy id."
    , command "spot-beacon" $
        info pSpotBeaconName $ progDesc "Calculate the Spot beacon name."
    , command "auction-beacon" $
        info pAuctionBeaconName $ progDesc "Calculate the Auction beacon name."
    , command "bid-beacon" $
        info pBidBeaconName $ progDesc "Calculate the Bid beacon name."
    , command "bidder-id" $
        info pBidderIdName $ progDesc "Calculate the BidderId name."
    , command "policy-beacon" $
        info pPolicyBeaconName $ progDesc "Calculate the PolicyBeacon name."
    ]
  where
    pBeaconPolicyId :: Parser Command
    pBeaconPolicyId = BeaconName BeaconPolicyId <$> pOutput

    pSpotBeaconName :: Parser Command
    pSpotBeaconName = BeaconName SpotBeaconName <$> pOutput

    pAuctionBeaconName :: Parser Command
    pAuctionBeaconName = BeaconName AuctionBeaconName <$> pOutput

    pBidBeaconName :: Parser Command
    pBidBeaconName = BeaconName BidBeaconName <$> pOutput

    pBidderIdName :: Parser Command
    pBidderIdName = 
      BeaconName 
        <$> (BidderIdName <$> pBidderCredential) 
        <*> pOutput

    pPolicyBeaconName :: Parser Command
    pPolicyBeaconName = 
      BeaconName 
        <$> (PolicyBeaconName <$> pNftPolicyId) 
        <*> pOutput

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmitTx :: Parser Command
pSubmitTx = 
  SubmitTx
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- EvaluateTx Parser
-------------------------------------------------
pEvaluateTx :: Parser Command
pEvaluateTx = 
  EvaluateTx 
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
  [ command "personal-address" $
      info pQueryPersonal $ progDesc "Query your personal address." 
  , command "protocol-params" $
      info pQueryParams $ progDesc "Query the current protocol parameters."
  , command "spots" $
      info pQuerySpots $ progDesc "Query open Spots for the protocol."
  , command "auctions" $
      info pQueryAuctions $ progDesc "Query open Auctions for the protocol."
  , command "bids" $
      info pQueryBids $ progDesc "Query open Bids for the protocol."
  ]

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pApiService
    <*> pBech32Address
    <*> pFormat
    <*> pOutput

pQueryParams :: Parser Query
pQueryParams =
  QueryParameters
    <$> pNetwork
    <*> pApiService
    <*> pOutput

pQuerySpots :: Parser Query
pQuerySpots =
  QuerySpots
    <$> pNetwork
    <*> pApiService
    <*> pNftPolicyId
    <*> ((Just <$> pSellerCredential) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryAuctions :: Parser Query
pQueryAuctions =
  QueryAuctions
    <$> pNetwork
    <*> pApiService
    <*> pNftPolicyId
    <*> ((Just <$> pSellerCredential) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryBids :: Parser Query
pQueryBids =
  QueryBids
    <$> pNetwork
    <*> pApiService
    <*> pNftPolicyId
    <*> ((Just <$> pSellerCredential) <|> pure Nothing)
    <*> ((Just <$> pBidderCredential) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "Save to file."
  <> completer (bashCompleter "file")
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet <|> pMainnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "For the preproduction testnet.")

    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "For the mainnet.")

pApiService :: Parser ApiService
pApiService = pure Koios
  -- where
  --   pKoios :: Parser Endpoint
  --   pKoios = flag' Koios
  --     (  long "koios"
  --     <> help "Use Koios."
  --     )
  --
  
pMarketRef :: Parser TxOutRef
pMarketRef = option (eitherReader readTxOutRef)
  (  long "market-ref"
  <> metavar "STRING"
  <> help "The output reference for the corresponding market input 'tx_hash#index'."
  )

pNftPolicyId :: Parser CurrencySymbol
pNftPolicyId = option (eitherReader readCurrencySymbol)
    (  long "nft-policy-id"
    <> metavar "STRING"
    <> help "The policy id for the nfts being sold."
    )

pNftName :: Parser TokenName
pNftName = option (eitherReader readTokenName)
    (  long "nft-name"
    <> metavar "STRING"
    <> help "The token name for the nft being sold."
    )

pPaymentAddress :: Parser Address
pPaymentAddress = 
  option (maybeReader $ rightToMaybe . paymentAddressToPlutusAddress . PaymentAddress . toText)
    (  long "payment-address"
    <> metavar "BECH32"
    <> help "The address where payments must go."
    )

pDeposit :: Parser Integer
pDeposit = option auto
  (  long "deposit"
  <> metavar "INT"
  <> help "The amount used for the minUTxOValue."
  )

pPrice :: Parser (Asset,Integer)
pPrice = option (eitherReader readAssetAmount)
  (  long "price"
  <> metavar "STRING"
  <> help "'# lovelace' or '# policy_id.asset_name'"
  )
    
pBidderCredential :: Parser Credential
pBidderCredential = pCredential "bidder"

pSellerCredential:: Parser Credential
pSellerCredential= pCredential "seller"

pCredential :: String -> Parser Credential
pCredential prefix = pStakingPubKeyCredential <|> pStakingScriptCredential
  where
    pStakingScriptCredential :: Parser Credential
    pStakingScriptCredential = ScriptCredential <$> option (eitherReader readScriptHash)
      (  long (prefix <> "-staking-script-hash")
      <> metavar "STRING"
      <> help ("The hash of the " <> prefix <> "'s staking script.")
      )

    pStakingPubKeyCredential :: Parser Credential
    pStakingPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long (prefix <> "-staking-pubkey-hash")
      <> metavar "STRING"
      <> help ("The hash of the " <> prefix <> "'s staking pubkey.")
      )

pTxFile :: Parser FilePath
pTxFile = strOption
  (  long "tx-file"
  <> metavar "STRING"
  <> help "Transaction file path."
  )

pFormat :: Parser Format
pFormat = pJSON <|> pPretty <|> pPlain
  where
    pJSON :: Parser Format
    pJSON = flag' JSON
      (  long "json"
      <> help "Format as JSON."
      )

    pPretty :: Parser Format
    pPretty = flag' Pretty
      (  long "pretty"
      <> help "Format for pretty-printing."
      )

    pPlain :: Parser Format
    pPlain = flag' Plain
      (  long "plain"
      <> help "Format for pretty-printing without colors."
      )

pBech32Address :: Parser PaymentAddress
pBech32Address = PaymentAddress <$> strOption
  (  long "address"
  <> metavar "BECH32"
  <> help "The target address."
  )
