{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotUTxOs.PurchaseSpotUTxOs.Regressions where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Basic Regression Tests
-------------------------------------------------
-- | Purchase a single valid Spot UTxO. The Spot UTxO has only one NFT for sale and only one asset 
-- for the sale price. The sale price includes ada. No beacons need to be changed. BurnBeacons is
-- used for the beacon script. All Spot UTxOs come from the same address.
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
      { tokens = flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                ]
            , mintRedeemer = toRedeemer BurnBeacons
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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
      , outputs = flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | Purchase multiple valid Spot UTxOs, each one for different nfts. The Spot UTxOs each have 
-- three NFTs for sale and thee assets for the sale price. The sale price includes ada. BurnBeacons
-- is used for the beacon script. All Spot UTxOs come from the same address.
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Sale Info
      spotDatum1 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      spotDatum2 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      spotDatum3 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken7" 
            , "TestToken8"
            , "TestToken9"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1)
    , ("TestToken2",1)
    , ("TestToken3",1)
    , ("TestToken4",1)
    , ("TestToken5",1)
    , ("TestToken6",1)
    , ("TestToken7",1)
    , ("TestToken8",1)
    , ("TestToken9",1)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken10",60)
    , ("TestToken11",60)
    ]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [spotDatum1,spotDatum2,spotDatum3] $ \SpotDatum{..} ->
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , outputs = flip map [spotDatum1, spotDatum2, spotDatum3] $ \spotDatum@SpotDatum{..} ->
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
      { tokens = flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                ]
            , mintRedeemer = toRedeemer BurnBeacons
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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
      , outputs = flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | Purchase a single valid Spot UTxO. The Spot UTxO has only one NFT for sale and only one asset 
-- for the sale price. The sale price includes ada. No beacons need to be changed. 
-- CreateCloseOrUpdateMarketUTxOs is used for the beacon script. All Spot UTxOs come from the 
-- same address.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
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
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

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
      { tokens = flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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
      , outputs = flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | Purchase multiple valid Spot UTxOs that are located at different addresses. 
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let -- Seller1 Info
      sellerWallet1 = Mock.knownMockWallet 1
      sellerPersonalAddr1 = Mock.mockWalletAddress sellerWallet1
      sellerPayPrivKey1 = Mock.paymentPrivateKey sellerWallet1
      sellerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet1
      sellerCred1 = PV2.PubKeyCredential sellerPubKey1
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred1
        }

      -- Seller2 Info
      sellerWallet2 = Mock.knownMockWallet 2
      sellerPersonalAddr2 = Mock.mockWalletAddress sellerWallet2
      -- sellerPayPrivKey2 = Mock.paymentPrivateKey sellerWallet2
      sellerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet2
      sellerCred2 = PV2.PubKeyCredential sellerPubKey2
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred2
        }

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 3
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Sale Info
      spotDatum1 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr1
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      spotDatum2 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr2
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet1 10_000_000 
    [ ("TestToken1",1)
    , ("TestToken2",1)
    , ("TestToken3",1)
    , ("TestToken4",1)
    , ("TestToken5",1)
    , ("TestToken6",1)
    , ("TestToken7",1)
    , ("TestToken8",1)
    , ("TestToken9",1)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken10",60)
    , ("TestToken11",60)
    ]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr1 [refScriptAddress] [sellerPayPrivKey1] $
    emptyTxParams
      { tokens = flip map [spotDatum1,spotDatum2] $ \SpotDatum{..} ->
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , outputs = mconcat
          [ flip map [spotDatum1] $ \spotDatum@SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
      }

  spotUTxOs <- (<>) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress1
                    <*> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress1,aftermarketAddress2,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                ]
            , mintRedeemer = toRedeemer BurnBeacons
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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
      , outputs = flip map spotUTxOs $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | Purchase multiple valid Spot UTxOs with unrelated outputs between the payment outputs.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let -- Seller1 Info
      sellerWallet1 = Mock.knownMockWallet 1
      sellerPersonalAddr1 = Mock.mockWalletAddress sellerWallet1
      sellerPayPrivKey1 = Mock.paymentPrivateKey sellerWallet1
      sellerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet1
      sellerCred1 = PV2.PubKeyCredential sellerPubKey1
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred1
        }

      -- Seller2 Info
      sellerWallet2 = Mock.knownMockWallet 2
      sellerPersonalAddr2 = Mock.mockWalletAddress sellerWallet2
      -- sellerPayPrivKey2 = Mock.paymentPrivateKey sellerWallet2
      sellerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet2
      sellerCred2 = PV2.PubKeyCredential sellerPubKey2
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred2
        }

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 3
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      -- Sale Info
      spotDatum1 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr1
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      spotDatum2 = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress sellerPersonalAddr2
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet1 10_000_000 
    [ ("TestToken1",1)
    , ("TestToken2",1)
    , ("TestToken3",1)
    , ("TestToken4",1)
    , ("TestToken5",1)
    , ("TestToken6",1)
    , ("TestToken7",1)
    , ("TestToken8",1)
    , ("TestToken9",1)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken10",60)
    , ("TestToken11",60)
    ]

  -- Try to create the Spot UTxO.
  void $ transact sellerPersonalAddr1 [refScriptAddress] [sellerPayPrivKey1] $
    emptyTxParams
      { tokens = flip map [spotDatum1,spotDatum2] $ \SpotDatum{..} ->
          TokenMint
            { mintTokens = 
                [ ("Spot", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
      , outputs = mconcat
          [ flip map [spotDatum1] $ \spotDatum@SpotDatum{..} ->
              Output
                { outputAddress = aftermarketAddress1
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Spot" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                    ]
                , outputDatum = OutputDatum $ toDatum spotDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , flip map [spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
      }

  spotUTxOs <- (<>) <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress1
                    <*> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress2

  -- Try to purchase the Spot UTxO.
  void $ transact buyerPersonalAddr [aftermarketAddress1,aftermarketAddress2,refScriptAddress] [buyerPayPrivKey]
    emptyTxParams
      { tokens = flip map spotUTxOs $ \(_, Just SpotDatum{..}) -> 
          TokenMint
            { mintTokens = 
                [ ("Spot", -1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                ]
            , mintRedeemer = toRedeemer BurnBeacons
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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
          [ [ Output
                { outputAddress = buyerPersonalAddr
                , outputValue = utxoValue 10_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          , flip map (take 1 spotUTxOs) $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
          , [ Output
                { outputAddress = buyerPersonalAddr
                , outputValue = utxoValue 10_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          , flip map (drop 1 spotUTxOs) $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
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

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all regression scenarios for purchasing Spot UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5
  ]
