{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.AcceptedBidUTxOs.CreateAcceptedBidUTxOs.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Creation Failures
-------------------------------------------------
-- | Try to create a single valid AcceptedBid UTxO using just the minting policy. The observer
-- script is not executed.
createFailure1 :: MonadEmulator m => m ()
createFailure1 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      -- sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
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

      -- Bid Info
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum@AcceptedBidDatum{..} =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = aftermarketAddress
              , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                  [ PV2.singleton beaconCurrencySymbol "Bid" 1
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | Try to create a single valid AcceptedBid UTxO using just the minting policy. The observer
-- script is executed.
createFailure2 :: MonadEmulator m => m ()
createFailure2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      -- sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
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

      -- Bid Info
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum@AcceptedBidDatum{..} =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = aftermarketAddress
              , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                  [ PV2.singleton beaconCurrencySymbol "Bid" 1
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
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
      , referenceInputs = [aftermarketObserverRef,beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | When purchasing a spot UTxO, try to create an AcceptedBid UTxO in the same transaction. The
-- AcceptedBid UTxO appears before the spot payment UTxO. The `BurnBeacons` redeemer is used for the
-- beacon policy.
createFailure3 :: MonadEmulator m => m ()
createFailure3 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      buyerCred = PV2.PubKeyCredential buyerPubKey

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
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = buyerCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [spotDatum] $ \SpotDatum{..} -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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

  spotUTxOs <- txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap spotUTxOs $ \(_, Just SpotDatum{..}) -> 
                      [ ("Spot", -1)
                      , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                      ]
                  , flip concatMap [bidDatum] $ \AcceptedBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer BurnBeacons
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map spotUTxOs $ \(spotRef,_) ->
          Input
            { inputId = spotRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer PurchaseSpot
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
      , outputs = mconcat
          [ flip map [bidDatum] $ \AcceptedBidDatum{..} ->
               Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
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
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | When purchasing a spot UTxO, try to create an AcceptedBid UTxO in the same transaction. The
-- AcceptedBid UTxO appears before the spot payment UTxO. The `CreateCloseOrUpdateMarketUTxOs` 
-- redeemer is used for the beacon policy.
createFailure4 :: MonadEmulator m => m ()
createFailure4 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      buyerCred = PV2.PubKeyCredential buyerPubKey

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
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = buyerCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [spotDatum] $ \SpotDatum{..} -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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

  spotUTxOs <- txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap spotUTxOs $ \(_, Just SpotDatum{..}) -> 
                      [ ("Spot", -1)
                      , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                      ]
                  , flip concatMap [bidDatum] $ \AcceptedBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map spotUTxOs $ \(spotRef,_) ->
          Input
            { inputId = spotRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer PurchaseSpot
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
      , outputs = mconcat
          [ flip map [bidDatum] $ \AcceptedBidDatum{..} ->
               Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
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
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | When purchasing a spot UTxO, try to create an AcceptedBid UTxO in the same transaction. The
-- AcceptedBid UTxO appears after the spot payment UTxO. The `BurnBeacons` redeemer is used for the
-- beacon policy.
createFailure5 :: MonadEmulator m => m ()
createFailure5 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      buyerCred = PV2.PubKeyCredential buyerPubKey

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
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = buyerCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [spotDatum] $ \SpotDatum{..} -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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

  spotUTxOs <- txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap spotUTxOs $ \(_, Just SpotDatum{..}) -> 
                      [ ("Spot", -1)
                      , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                      ]
                  , flip concatMap [bidDatum] $ \AcceptedBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer BurnBeacons
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map spotUTxOs $ \(spotRef,_) ->
          Input
            { inputId = spotRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer PurchaseSpot
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
      , outputs = reverse $ mconcat
          [ flip map [bidDatum] $ \AcceptedBidDatum{..} ->
               Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
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
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | When purchasing a spot UTxO, try to create an AcceptedBid UTxO in the same transaction. The
-- AcceptedBid UTxO appears after the spot payment UTxO. The `CreateCloseOrUpdateMarketUTxOs` 
-- redeemer is used for the beacon policy.
createFailure6 :: MonadEmulator m => m ()
createFailure6 = do
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      buyerCred = PV2.PubKeyCredential buyerPubKey

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
      tmpDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = buyerCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }
      bidDatum =
        createAcceptedBidDatumFromClaimBid 
          0 
          (toPlutusAddress sellerPersonalAddr) 
          tmpDatum

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [spotDatum] $ \SpotDatum{..} -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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

  spotUTxOs <- txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap spotUTxOs $ \(_, Just SpotDatum{..}) -> 
                      [ ("Spot", -1)
                      , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                      ]
                  , flip concatMap [bidDatum] $ \AcceptedBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map spotUTxOs $ \(spotRef,_) ->
          Input
            { inputId = spotRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer PurchaseSpot
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
      , outputs = reverse $ mconcat
          [ flip map [bidDatum] $ \AcceptedBidDatum{..} ->
               Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    ]
                , outputDatum = OutputDatum $ toDatum bidDatum
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
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | When accepting a claim bid, create an extra AcceptedBid UTxO. The AcceptedBid UTxO appears
-- before the required AcceptedBid output. The beacon policy is executed using
-- `CreateCloseOrUpdateMarketUTxOs`.
createFailure7 :: MonadEmulator m => m ()
createFailure7 = do
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

      -- Bid Info
      bidDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \ClaimBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [bidDatum] $ \datum@ClaimBidDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

  bidUTxOs <- txOutRefsAndDatumsAtAddress @ClaimBidDatum aftermarketAddress
  startSlot <- currentSlot

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap [bidDatum] $ \ClaimBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer $ AcceptClaimBid 0 (toPlutusAddress sellerPersonalAddr)
            }
      , outputs = mconcat
          [ flip map bidUTxOs $ \(_ , Just datum) ->
              let acceptedDatum@AcceptedBidDatum{..} = createAcceptedBidDatumFromClaimBid 
                      0 
                      (toPlutusAddress sellerPersonalAddr) 
                      datum
               in Output
                  { outputAddress = aftermarketAddress
                  , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                      [ PV2.singleton beaconCurrencySymbol "Bid" 1
                      , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                      , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                      ]
                  , outputDatum = OutputDatum $ toDatum acceptedDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
          , flip map bidUTxOs $ \(_, Just datum@ClaimBidDatum{..}) ->
              Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptedBidDatumFromClaimBid 
                      0 
                      (toPlutusAddress sellerPersonalAddr) 
                      datum
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | When accepting a claim bid, create an extra AcceptedBid UTxO. The AcceptedBid UTxO appears
-- after the required AcceptedBid output. The beacon policy is executed using
-- `CreateCloseOrUpdateMarketUTxOs`.
createFailure8 :: MonadEmulator m => m ()
createFailure8 = do
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

      -- Bid Info
      bidDatum = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum] $ \ClaimBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip map [bidDatum] $ \datum@ClaimBidDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Bid" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

  bidUTxOs <- txOutRefsAndDatumsAtAddress @ClaimBidDatum aftermarketAddress
  startSlot <- currentSlot

  -- Try to accept the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = mconcat
                  [ flip concatMap [bidDatum] $ \ClaimBidDatum{..} ->
                      [ (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                      , ("Bid", 1)
                      , (unBidderId $ toBidderId bidderCredential, 1)
                      ]
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer $ AcceptClaimBid 0 (toPlutusAddress sellerPersonalAddr)
            }
      , outputs = reverse $ mconcat
          [ flip map bidUTxOs $ \(_ , Just datum) ->
              let acceptedDatum@AcceptedBidDatum{..} = createAcceptedBidDatumFromClaimBid 
                      0 
                      (toPlutusAddress sellerPersonalAddr) 
                      datum
               in Output
                  { outputAddress = aftermarketAddress
                  , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                      [ PV2.singleton beaconCurrencySymbol "Bid" 1
                      , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                      , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                      ]
                  , outputDatum = OutputDatum $ toDatum acceptedDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
          , flip map bidUTxOs $ \(_, Just datum@ClaimBidDatum{..}) ->
              Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map nftNames $ \name -> 
                        PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptedBidDatumFromClaimBid 
                      0 
                      (toPlutusAddress sellerPersonalAddr) 
                      datum
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for creating AcceptedBid UTxOs.
tests :: [TestTree]
tests =
  [ -- Creation Failures
    scriptMustFailWithError "createFailure1"
      "Observer script not executed"
      createFailure1
  , scriptMustFailWithError "createFailure2"
      "Extra AcceptedBid UTxO found"
      createFailure2
  , scriptMustFailWithError "createFailure3"
      "This redeemer can only be used to burn beacons"
      createFailure3
  , scriptMustFailWithError "createFailure4"
      "Extra AcceptedBid UTxO found"
      createFailure4
  , scriptMustFailWithError "createFailure5"
      "This redeemer can only be used to burn beacons"
      createFailure5
  , scriptMustFailWithError "createFailure6"
      "Extra AcceptedBid UTxO found"
      createFailure6
  , scriptMustFailWithError "createFailure7"
      "AcceptedBid UTxO has wrong value"
      createFailure7
  , scriptMustFailWithError "createFailure8"
      "Extra AcceptedBid UTxO found"
      createFailure8
  ]
