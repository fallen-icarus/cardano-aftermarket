{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Misc where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.List (intersperse)
import Data.Maybe (isJust)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Misc Regression Tests
-------------------------------------------------
-- | Accept a single valid SpotBid UTxO and close the associated Auction UTxO in the same transaction.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
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

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

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

-- | Accept a single valid SpotBid UTxO and close the associated Spot UTxO in the same transaction.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
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

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [spotDatum] $ \SpotDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Spot" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum spotDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
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

-- | Accept a single valid SpotBid UTxO, close the associated Auction UTxO, and purchase a Spot UTxO in
-- the same transaction.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash bidderCred
        }

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken2" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress1
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO and the other Spot UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum] $ \SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress2
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress1
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  -- Try the composition.
  void $ transact 
    sellerPersonalAddr 
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [sellerPayPrivKey]
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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer PurchaseSpot
                }
          ]
      , outputs = mconcat
          [ flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
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
-- Misc Failure Tests
-------------------------------------------------
-- | When accepting a single valid SpotBid UTxO, close the associated Auction UTxO, and purchase a Spot UTxO in
-- the same transaction, the auction payment output is first but invalid. There is an extra output
-- between the payment outputs and first in the output list.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash bidderCred
        }

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken2" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress1
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO and the other Spot UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum] $ \SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress2
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress1
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  let extraOutput = 
        Output
          { outputAddress = sellerPersonalAddr
          , outputValue = utxoValue 10_000_000 mempty
          , outputDatum = NoOutputDatum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Try the composition.
  void $ transact 
    sellerPersonalAddr 
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [sellerPayPrivKey]
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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer PurchaseSpot
                }
          ]
      , outputs = intersperse extraOutput $ mconcat
          [ [extraOutput]
          , flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ bidDeposit - 1) $ mconcat
                    [ mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
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

-- | When accepting a single valid SpotBid UTxO, close the associated Auction UTxO, and purchase a Spot UTxO in
-- the same transaction, the spot payment output is second but invalid. There is an extra output
-- between the payment outputs and first in the output list.
failureTest2 :: MonadEmulator m => m ()
failureTest2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash bidderCred
        }

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken2" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress1
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO and the other Spot UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum] $ \SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress2
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress1
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  let extraOutput = 
        Output
          { outputAddress = sellerPersonalAddr
          , outputValue = utxoValue 10_000_000 mempty
          , outputDatum = NoOutputDatum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Try the composition.
  void $ transact 
    sellerPersonalAddr 
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [sellerPayPrivKey]
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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer PurchaseSpot
                }
          ]
      , outputs = intersperse extraOutput $ mconcat
          [ [extraOutput]
          , flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
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

-- | When accepting a single valid SpotBid UTxO, close the associated Auction UTxO, and purchase a Spot UTxO in
-- the same transaction, the auction payment output is second but invalid. There is an extra output
-- between the payment outputs and first in the output list.
failureTest3 :: MonadEmulator m => m ()
failureTest3 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash bidderCred
        }

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken2" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress1
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO and the other Spot UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum] $ \SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress2
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress1
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  let extraOutput = 
        Output
          { outputAddress = sellerPersonalAddr
          , outputValue = utxoValue 10_000_000 mempty
          , outputDatum = NoOutputDatum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Try the composition.
  void $ transact 
    sellerPersonalAddr 
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [sellerPayPrivKey]
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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer PurchaseSpot
                }
          ]
      , outputs = intersperse extraOutput $ mconcat
          [ [extraOutput]
          , flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ bidDeposit - 1) $ mconcat
                    [ mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
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

-- | When accepting a single valid SpotBid UTxO, close the associated Auction UTxO, and purchase a Spot UTxO in
-- the same transaction, the spot payment output is first but invalid. There is an extra output
-- between the payment outputs and first in the output list.
failureTest4 :: MonadEmulator m => m ()
failureTest4 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash bidderCred
        }

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Sale Info
      spotDatum = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken2" ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      -- Bid Info
      bidDatum = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [auctionDatum] $ \AuctionDatum{..} ->
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [auctionDatum] $ \datum@AuctionDatum{..} ->
          Output
            { outputAddress = aftermarketAddress1
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      }

  -- Try to create the Bid UTxO and the other Spot UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          , TokenMint
              { mintTokens = flip concatMap [spotDatum] $ \SpotDatum{..} ->
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum datum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum] $ \SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress2
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
  auctionUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress1
  spotUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  let extraOutput = 
        Output
          { outputAddress = sellerPersonalAddr
          , outputValue = utxoValue 10_000_000 mempty
          , outputDatum = NoOutputDatum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Try the composition.
  void $ transact 
    sellerPersonalAddr 
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [sellerPayPrivKey]
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
          , flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", -1)
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
          , flip map spotUTxOs $ \(spotRef,_) ->
              Input
                { inputId = spotRef
                , inputWitness = 
                    SpendWithPlutusReference aftermarketRef InlineDatum $ 
                      toRedeemer PurchaseSpot
                }
          ]
      , outputs = intersperse extraOutput $ mconcat
          [ [extraOutput]
          , flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map bidUTxOs $ \(ref, Just SpotBidDatum{..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          ]
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
-- | A `TestTree` containing all miscelleneous scenarios. These include complicated compositions and
-- edge-cases.
tests :: TestTree
tests = testGroup "Misc Tests"
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3

  , scriptMustFailWithError "failureTest1"
      "Spot auction payment UTxO missing bid deposit"
      failureTest1
  , scriptMustFailWithError "failureTest2"
      "Spot payment UTxO has wrong value"
      failureTest2
  , scriptMustFailWithError "failureTest3"
      "Spot auction payment UTxO missing bid deposit"
      failureTest3
  , scriptMustFailWithError "failureTest4"
      "Spot payment UTxO has wrong value"
      failureTest4
  ]
