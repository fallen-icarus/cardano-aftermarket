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
  , command "convert-time" $
      info pConvertTime $ progDesc "Convert POSIXTime <--> Slot."
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
          <|> pObserverScript
          <|> pMarketScript
          <|> pProxyScript

    pBeaconScript :: Parser Script
    pBeaconScript = flag' BeaconScript
      (  long "beacon-script"
      <> help "Export the beacon script."
      )

    pObserverScript :: Parser Script
    pObserverScript = flag' MarketObserverScript
      (  long "observer-script"
      <> help "Export the market observer script."
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
  , command "spot-bid" $
      info pCreateNewSpotBidInfo $ progDesc "Create a SpotBidDatum."
  , command "claim-bid" $
      info pCreateNewClaimBidInfo $ progDesc "Create a ClaimBidDatum."
  , command "accepted-bid" $
      info pCreateAcceptedBidDatum $ progDesc "Create an AcceptedBidDatum."
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
        <*> pDeposit "seller"
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

pCreateNewSpotBidInfo :: Parser Command
pCreateNewSpotBidInfo =
    CreateDatum
      <$> pNewSpotBidInfo
      <*> pOutputFile
  where
    pNewSpotBidInfo :: Parser NewDatum
    pNewSpotBidInfo =
      fmap NewSpotBidDatum $ NewSpotBidInfo
        <$> pNftPolicyId
        <*> pBidderCredential
        <*> some pNftName
        <*> pPaymentAddress
        <*> pDeposit "bidder"
        <*> some pPrice

pCreateNewClaimBidInfo :: Parser Command
pCreateNewClaimBidInfo =
    CreateDatum
      <$> pNewClaimBidInfo
      <*> pOutputFile
  where
    pNewClaimBidInfo :: Parser NewDatum
    pNewClaimBidInfo =
      fmap NewClaimBidDatum $ NewClaimBidInfo
        <$> pNftPolicyId
        <*> pBidderCredential
        <*> some pNftName
        <*> pDeposit "bidder"
        <*> some pPrice
        <*> pBidExpiration
        <*> pClaimExpiration

pCreateAcceptedBidDatum :: Parser Command
pCreateAcceptedBidDatum = hsubparser $ mconcat
    [ command "manual" $
        info pCreateAcceptedBidManual $ 
          progDesc "Create an AcceptedBidDatum manually."
    , command "auto" $
        info pCreateAcceptedBidAuto $ 
          progDesc "Create an AcceptedBidDatum by looking up the ClaimBid UTxO."
    ]

pCreateAcceptedBidManual :: Parser Command
pCreateAcceptedBidManual =
    CreateDatum
      <$> pNewAcceptedBidInfo
      <*> pOutputFile
  where
    pNewAcceptedBidInfo :: Parser NewDatum
    pNewAcceptedBidInfo =
      fmap NewAcceptedBidDatumManual $ NewAcceptedBidInfo
        <$> pNftPolicyId
        <*> pBidderCredential
        <*> some pNftName
        <*> pDeposit "bidder"
        <*> pDeposit "seller"
        <*> some pPrice
        <*> pClaimExpiration
        <*> pPaymentAddress

pCreateAcceptedBidAuto :: Parser Command
pCreateAcceptedBidAuto =
    CreateDatum
      <$> pNewAcceptedBid
      <*> pOutputFile
  where
    pNewAcceptedBid :: Parser NewDatum
    pNewAcceptedBid =
      NewAcceptedBidDatumAuto
        <$> pNetwork
        <*> pApiService
        <*> pBidRef
        <*> pDeposit "seller"
        <*> pPaymentAddress

-------------------------------------------------
-- CreateRedeemer Parser
-------------------------------------------------
parseCreateRedeemer :: Parser Command
parseCreateRedeemer = hsubparser $ mconcat
    [ command "beacon-script" $
        info pBeaconRedeemer $ progDesc "Create a redeemer for the beacon script."
    , command "market-script" $
        info pMarketRedeemer $ progDesc "Create a redeemer for the aftermarket script."
    , command "observer-script" $
        info pMarketObserverRedeemer $ progDesc "Create a redeemer for the observer script."
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
        
pMarketObserverRedeemer :: Parser Command
pMarketObserverRedeemer = hsubparser $ mconcat
    [ command "observe" $
        info pObserveMarket $ 
          progDesc "Create the redeemer for observing a market action."
    , command "register" $
        info pRegisterMarketObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObserveMarket :: Parser Command
    pObserveMarket = 
      CreateRedeemer 
        <$> pure (NewMarketObserverRedeemer $ ObserveAftermarket $ BeaconId beaconCurrencySymbol)
        <*> pOutputFile

    pRegisterMarketObserverScript :: Parser Command
    pRegisterMarketObserverScript =
      CreateRedeemer 
        <$> pure (NewMarketObserverRedeemer RegisterAftermarketObserverScript)
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
    , command "accept-spot-bid" $
        info pAcceptSpotBid $ 
          progDesc "Create the redeemer for accepting a SpotBid UTxO."
    , command "accept-claim-bid" $
        info pAcceptClaimBid $ 
          progDesc "Create the redeemer for accepting a ClaimBid UTxO."
    , command "claim-accepted-bid" $
        info pClaimAcceptedBid $ 
          progDesc "Create the redeemer for claiming an AcceptedBid UTxO."
    , command "unlock" $
        info pUnlock $ 
          progDesc "Create the redeemer for unlocking an unclaimed AcceptedBid UTxO."
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

    pAcceptSpotBid :: Parser Command
    pAcceptSpotBid = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer AcceptSpotBid)
        <*> pOutputFile

    pAcceptClaimBid :: Parser Command
    pAcceptClaimBid = 
      CreateRedeemer
        <$> (NewMarketRedeemer <$> (AcceptClaimBid <$> pDeposit "seller" <*> pPaymentAddress))
        <*> pOutputFile

    pClaimAcceptedBid :: Parser Command
    pClaimAcceptedBid = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer ClaimAcceptedBid)
        <*> pOutputFile

    pUnlock :: Parser Command
    pUnlock = 
      CreateRedeemer
        <$> pure (NewMarketRedeemer UnlockUnclaimedAcceptedBid)
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
-- ConvertTime Parser
-------------------------------------------------
pConvertTime :: Parser Command
pConvertTime = ConvertTime <$> (pPOSIXTime <|> pSlot) <*> pNetwork
  where
    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time (in milliseconds) to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

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
    <> help "The address where the payment must go."
    )

pDeposit :: String -> Parser Integer
pDeposit user = option auto
  (  long (user <> "-deposit")
  <> metavar "INT"
  <> help ("The amount used by the " <> user <> " for the minUTxOValue.")
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

pBidRef :: Parser TxOutRef
pBidRef = option (eitherReader readTxOutRef)
  (  long "bid-ref"
  <> metavar "STRING"
  <> help "The output reference for the target bid 'tx_hash#index'."
  )

pBidExpiration :: Parser (Maybe POSIXTime)
pBidExpiration = pNoExpiration <|> pExpiration
  where
    pNoExpiration = flag' Nothing
      (  long "no-bid-expiration"
      <> help "The bid does not expire."
      )
    pExpiration = Just . POSIXTime <$> option auto
      (  long "bid-expiration"
      <> metavar "TIME"
      <> help "The expiration time for the bid in POSIX time (milliseconds)."
      )

pClaimExpiration :: Parser POSIXTime
pClaimExpiration = POSIXTime <$> option auto
  (  long "claim-expiration"
  <> metavar "TIME"
  <> help "The expiration time for the claim period in POSIX time (milliseconds)."
  )

