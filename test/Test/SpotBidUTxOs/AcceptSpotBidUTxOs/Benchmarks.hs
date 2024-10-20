{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotBidUTxOs.AcceptSpotBidUTxOs.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)
import Data.List (sort)
import Control.Monad (forM_)
import Data.Maybe (isJust)
import Optics.Operators
import Optics.Optic

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Benchmarks
-------------------------------------------------
-- | Accept multiple valid Bid UTxOs. The bids are for three NFTs and uses three assets
-- for the bid. BurnBeacons is used to burn the beacons. The Auction UTxOs are closed separately.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      assets = 
        map (\i -> (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..124]

      -- Bid Info
      datums = flip map (grouped 3 assets) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1"
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = sort $ zip (map Asset xs) $ repeat 20
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 $ zip (map snd assets) $ repeat 20
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1", fromIntegral number)
    , ("TestToken2", fromIntegral number)
    , ("TestToken3", fromIntegral number)
    ]

  -- Try to create the Bid UTxO.
  forM_ (grouped 15 datums) $ \bidDatums ->
    transact 
    bidderPersonalAddr 
    [refScriptAddress] 
    [bidderPayPrivKey]
    emptyTxParams
      { tokens = flip map bidDatums $ \SpotBidDatum{..} ->
          TokenMint
            { mintTokens = 
                [ ("Bid", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                , (unBidderId $ toBidderId bidderCredential, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , outputs = flip map bidDatums $ \bidDatum@SpotBidDatum{bid=Prices bid,..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum bidDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = flip map bidUTxOs $ \(_, Just SpotBidDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Bid", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                , (unBidderId $ toBidderId bidderCredential, -1)
                ]
            , mintRedeemer = toRedeemer BurnBeacons
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer AcceptSpotBid
            }
      , outputs = flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ mconcat $ flip map nftNames $ \name -> 
                    PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential aftermarketObserverScriptHash
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference aftermarketObserverRef $ 
                    toRedeemer $ ObserveAftermarket $ BeaconId beaconCurrencySymbol
              }
          ]
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Accept multiple valid Bid UTxOs. The bids are for three NFTs and uses three assets
-- for the bid. CreateCloseOrUpdateMarketUTxOs is used to burn the beacons. The Auction UTxOs are
-- closed separately.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      assets = 
        map (\i -> (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..124]

      -- Bid Info
      datums = flip map (grouped 3 assets) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1"
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = sort $ zip (map Asset xs) $ repeat 20
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 $ zip (map snd assets) $ repeat 20
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1", fromIntegral number)
    , ("TestToken2", fromIntegral number)
    , ("TestToken3", fromIntegral number)
    ]

  -- Try to create the Bid UTxO.
  forM_ (grouped 15 datums) $ \bidDatums ->
    transact 
    bidderPersonalAddr 
    [refScriptAddress] 
    [bidderPayPrivKey]
    emptyTxParams
      { tokens = flip map bidDatums $ \SpotBidDatum{..} ->
          TokenMint
            { mintTokens = 
                [ ("Bid", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                , (unBidderId $ toBidderId bidderCredential, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , outputs = flip map bidDatums $ \bidDatum@SpotBidDatum{bid=Prices bid,..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum bidDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = flip map bidUTxOs $ \(_, Just SpotBidDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Bid", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                , (unBidderId $ toBidderId bidderCredential, -1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer AcceptSpotBid
            }
      , outputs = flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ mconcat $ flip map nftNames $ \name -> 
                    PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential aftermarketObserverScriptHash
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference aftermarketObserverRef $ 
                    toRedeemer $ ObserveAftermarket $ BeaconId beaconCurrencySymbol
              }
          ]
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Accept multiple valid Bid UTxOs, each one for different nfts. The Auction UTxOs each have 
-- three NFTs for sale and thee assets for the sale price. The Auction UTxOs are closed in the same
-- transaction. BurnBeacons is used to burn the beacons.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      auctionDatums = 
        flip map (grouped 3 nfts) $ \xs -> unsafeCreateAuctionDatum $ NewAuctionInfo
          { nftPolicyId = testTokenSymbol
          , nftNames = sort xs
          , startingPrice = 
              [ (Asset (adaSymbol,adaToken), 20_000_000) 
              , (Asset (testTokenSymbol,"TestToken1"), 20) 
              , (Asset (testTokenSymbol,"TestToken2"), 20) 
              ]
          }

      -- Bid Info
      bidDatums = flip map (grouped 3 nfts) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = sort xs
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken1"), 20) 
            , (Asset (testTokenSymbol,"TestToken2"), 20) 
            ]
        }

      allDatums = zip auctionDatums bidDatums

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1", 41 * 20)
    , ("TestToken2", 41 * 20)
    ]

  -- Try to create the Auction UTxO.
  forM_ (grouped 5 allDatums) $ \datums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey,bidderPayPrivKey]
      emptyTxParams
        { tokens = flip concatMap datums $ \(auctionDatum,bidDatum) ->
            [ TokenMint
                { mintTokens =
                    [ ("Auction", 1)
                    , (unPolicyBeacon $ toPolicyBeacon $ auctionDatum ^. #nftPolicyId, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
            , TokenMint
                { mintTokens =
                    [ ("Bid", 1)
                    , (unPolicyBeacon $ toPolicyBeacon $ bidDatum ^. #nftPolicyId, 1)
                    , (unBidderId $ toBidderId $ bidDatum ^. #bidderCredential, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
            ]
        , outputs = flip concatMap datums $ \(auctionDatum,bidDatum) ->
            [ Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue 5_000_000 $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Auction" 1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unPolicyBeacon $ toPolicyBeacon $ auctionDatum ^. #nftPolicyId) 
                        1
                    , mconcat $ flip map (auctionDatum ^. #nftNames) $ \name -> 
                        PV2.singleton (auctionDatum ^. #nftPolicyId) name 1
                    ]
                , outputDatum = OutputDatum $ toDatum auctionDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral $ bidDatum ^. #bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unPolicyBeacon $ toPolicyBeacon $ bidDatum ^. #nftPolicyId) 
                        1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unBidderId $ toBidderId $ bidDatum ^. #bidderCredential) 
                        1
                    , mconcat $ flip map (bidDatum ^. #bid % #unPrices) $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [beaconsRef]
        , extraKeyWitnesses = [bidderPubKey]
        }

  bidUTxOs <- take number . filter (isJust . snd) <$> 
    txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress
  auctionUTxOs <- take number . filter (isJust . snd) <$> 
    txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = mconcat
          [ flip map bidUTxOs $ \(_, Just SpotBidDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Bid", -1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                    , (unBidderId $ toBidderId bidderCredential, -1)
                    ]
                , mintRedeemer = toRedeemer BurnBeacons
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          , flip map auctionUTxOs $ \(_, Just AuctionDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Auction", -1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                    ]
                , mintRedeemer = toRedeemer BurnBeacons
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          ]
      , inputs = mconcat
          [ flip map bidUTxOs $ \(bidRef,_) ->
              Input
                { inputId = bidRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer AcceptSpotBid
                }
          , flip map auctionUTxOs $ \(auctionRef,_) ->
              Input
                { inputId = auctionRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer AcceptSpotBid
                }
          ]
      , outputs = flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ mconcat $ flip map nftNames $ \name -> 
                    PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential aftermarketObserverScriptHash
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference aftermarketObserverRef $ 
                    toRedeemer $ ObserveAftermarket $ BeaconId beaconCurrencySymbol
              }
          ]
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Accept multiple valid Bid UTxOs, each one for different nfts. The Auction UTxOs each have 
-- three NFTs for sale and thee assets for the sale price. The Auction UTxOs are closed in the same
-- transaction. CreateCloseOrUpdateMarketUTxOs is used to burn the beacons.
benchTest4 :: MonadEmulator m => Int -> m ()
benchTest4 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      auctionDatums = 
        flip map (grouped 3 nfts) $ \xs -> unsafeCreateAuctionDatum $ NewAuctionInfo
          { nftPolicyId = testTokenSymbol
          , nftNames = sort xs
          , startingPrice = 
              [ (Asset (adaSymbol,adaToken), 20_000_000) 
              , (Asset (testTokenSymbol,"TestToken1"), 20) 
              , (Asset (testTokenSymbol,"TestToken2"), 20) 
              ]
          }

      -- Bid Info
      bidDatums = flip map (grouped 3 nfts) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = sort xs
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken1"), 20) 
            , (Asset (testTokenSymbol,"TestToken2"), 20) 
            ]
        }

      allDatums = zip auctionDatums bidDatums

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1", 41 * 20)
    , ("TestToken2", 41 * 20)
    ]

  -- Try to create the Auction UTxO.
  forM_ (grouped 5 allDatums) $ \datums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey,bidderPayPrivKey]
      emptyTxParams
        { tokens = flip concatMap datums $ \(auctionDatum,bidDatum) ->
            [ TokenMint
                { mintTokens =
                    [ ("Auction", 1)
                    , (unPolicyBeacon $ toPolicyBeacon $ auctionDatum ^. #nftPolicyId, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
            , TokenMint
                { mintTokens =
                    [ ("Bid", 1)
                    , (unPolicyBeacon $ toPolicyBeacon $ bidDatum ^. #nftPolicyId, 1)
                    , (unBidderId $ toBidderId $ bidDatum ^. #bidderCredential, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
            ]
        , outputs = flip concatMap datums $ \(auctionDatum,bidDatum) ->
            [ Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue 5_000_000 $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Auction" 1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unPolicyBeacon $ toPolicyBeacon $ auctionDatum ^. #nftPolicyId) 
                        1
                    , mconcat $ flip map (auctionDatum ^. #nftNames) $ \name -> 
                        PV2.singleton (auctionDatum ^. #nftPolicyId) name 1
                    ]
                , outputDatum = OutputDatum $ toDatum auctionDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral $ bidDatum ^. #bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unPolicyBeacon $ toPolicyBeacon $ bidDatum ^. #nftPolicyId) 
                        1
                    , PV2.singleton 
                        beaconCurrencySymbol 
                        (unBidderId $ toBidderId $ bidDatum ^. #bidderCredential) 
                        1
                    , mconcat $ flip map (bidDatum ^. #bid % #unPrices) $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [beaconsRef]
        , extraKeyWitnesses = [bidderPubKey]
        }

  bidUTxOs <- take number . filter (isJust . snd) <$> 
    txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress
  auctionUTxOs <- take number . filter (isJust . snd) <$> 
    txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = mconcat
          [ flip map bidUTxOs $ \(_, Just SpotBidDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Bid", -1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                    , (unBidderId $ toBidderId bidderCredential, -1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          , flip map auctionUTxOs $ \(_, Just AuctionDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Auction", -1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          ]
      , inputs = mconcat
          [ flip map bidUTxOs $ \(bidRef,_) ->
              Input
                { inputId = bidRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer AcceptSpotBid
                }
          , flip map auctionUTxOs $ \(auctionRef,_) ->
              Input
                { inputId = auctionRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer CloseOrUpdateSellerUTxO
                }
          ]
      , outputs = flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ mconcat $ flip map nftNames $ \name -> 
                    PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential aftermarketObserverScriptHash
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference aftermarketObserverRef $ 
                    toRedeemer $ ObserveAftermarket $ BeaconId beaconCurrencySymbol
              }
          ]
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for purchasing Spot UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 18
  , mustSucceed "benchTest2" $ benchTest2 18
  , mustSucceed "benchTest3" $ benchTest3 10
  , mustSucceed "benchTest4" $ benchTest4 11

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 19
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 19
  , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 11
  , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 12
  ]
