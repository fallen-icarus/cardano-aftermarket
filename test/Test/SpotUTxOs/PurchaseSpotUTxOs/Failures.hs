{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotUTxOs.PurchaseSpotUTxOs.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | When purchasing a Spot UTxO, do not burn the beacons. The beacon script is not executed.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
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
                [ ("Spot", 0)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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

-- | When purchasing a Spot UTxO, do not burn the beacons. The beacon script is executed as a
-- staking script using CreateCloseOrUpdateMarketUTxOs.
beaconFailure2 :: MonadEmulator m => m ()
beaconFailure2 = do
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
                [ ("Spot", 0)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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
          , Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference beaconsRef $ 
                    toRedeemer CreateCloseOrUpdateMarketUTxOs
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

-- | When purchasing a Spot UTxO, do not burn the beacons. The beacon script is executed as a
-- staking script using BurnBeacons.
beaconFailure3 :: MonadEmulator m => m ()
beaconFailure3 = do
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
                [ ("Spot", 0)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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
          , Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference beaconsRef $ 
                    toRedeemer BurnBeacons
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

-- | When purchasing a Spot UTxO, do not burn all of the beacons. The beacon script is executed with
-- BurnBeacons.
beaconFailure4 :: MonadEmulator m => m ()
beaconFailure4 = do
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
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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

-- | When purchasing a Spot UTxO, do not burn all of the beacons. The beacon script is executed with
-- CreateCloseOrUpdateMarketUTxOs.
beaconFailure5 :: MonadEmulator m => m ()
beaconFailure5 = do
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
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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

-- | When purchasing a Spot UTxO, mint extra beacons using BurnBeacons.
beaconFailure6 :: MonadEmulator m => m ()
beaconFailure6 = do
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
                , ("Other", 1)
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

-- | Purchase multiple valid Spot UTxOs that are located at different addresses, but only burn some
-- of the beacons.
beaconFailure7 :: MonadEmulator m => m ()
beaconFailure7 = do
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
                [ ("Spot", 0)
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

-- | Try to purchase a single invalid Spot UTxO. 
beaconFailure8 :: MonadEmulator m => m ()
beaconFailure8 = do
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
      { tokens = []
      , outputs = flip map [spotDatum] $ \SpotDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1 ]
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
                [ ("Spot", 0)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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

-------------------------------------------------
-- Payment Value Failures
-------------------------------------------------
-- | The payment output does not have enough of the assets from the sale price. Ada is part of the
-- sale price.
paymentValueFailure1 :: MonadEmulator m => m ()
paymentValueFailure1 = do
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
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), _) -> 
                    PV2.singleton sym name 10_000_000
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output does not have enough of the assets from the sale price. Ada is not part of the
-- sale price.
paymentValueFailure2 :: MonadEmulator m => m ()
paymentValueFailure2 = do
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
            [ (Asset (testTokenSymbol,"TestToken2"), 20) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken2",20)]

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
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), _) -> 
                    PV2.singleton sym name 10
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output does not have the sale deposit. Ada is not part of the sale price.
paymentValueFailure3 :: MonadEmulator m => m ()
paymentValueFailure3 = do
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
            [ (Asset (testTokenSymbol,"TestToken2"), 20) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken2",20)]

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
            , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output does not have all of the required assets from the sale price. Ada is part of
-- the sale price.
paymentValueFailure4 :: MonadEmulator m => m ()
paymentValueFailure4 = do
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
                [ mconcat $ flip map (take 2 salePrice) $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-------------------------------------------------
-- Payment Datum Failures
-------------------------------------------------
-- | The payment output has the wrong beacon id in the PaymentDatum.
paymentDatumFailure1 :: MonadEmulator m => m ()
paymentDatumFailure1 = do
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
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId "",ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output has the wrong output reference in the PaymentDatum.
paymentDatumFailure2 :: MonadEmulator m => m ()
paymentDatumFailure2 = do
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
      , outputs = flip map spotUTxOs $ \(_, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ 
                PaymentDatum (BeaconId beaconCurrencySymbol,beaconsRef)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output does not have an inline PaymentDatum.
paymentDatumFailure3 :: MonadEmulator m => m ()
paymentDatumFailure3 = do
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
            , outputDatum = OutputDatumHash $ datumHash $ 
                PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-- | The payment output does not have a datum.
paymentDatumFailure4 :: MonadEmulator m => m ()
paymentDatumFailure4 = do
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
      , outputs = flip map spotUTxOs $ \(_, Just SpotDatum{salePrice=Prices salePrice,..}) ->
          Output
            { outputAddress = toCardanoApiAddress paymentAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-------------------------------------------------
-- Payment Address Failures
-------------------------------------------------
-- | The payment output is to the wrong address.
paymentAddressFailure1 :: MonadEmulator m => m ()
paymentAddressFailure1 = do
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
            { outputAddress = buyerPersonalAddr
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                    PV2.singleton sym name num
                ]
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      }

-------------------------------------------------
-- Payment Output Order Failures
-------------------------------------------------
-- | When purchasing multiple Spot UTxOs, the payment outputs are not in the same order as the
-- inputs.
orderFailure1 :: MonadEmulator m => m ()
orderFailure1 = do
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
      , outputs = flip map (reverse spotUTxOs) $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
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

-- | When purchasing multiple Spot UTxOs, the first payment output is invalid.
orderFailure2 :: MonadEmulator m => m ()
orderFailure2 = do
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
      , outputs = flip map [spotDatum1, spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
      , outputs = flip map (zip spotUTxOs [(1::Int)..]) $ 
          \((ref, Just SpotDatum{salePrice=Prices salePrice,..}),i) ->
            if odd i then
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
            else 
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

-- | When purchasing multiple Spot UTxOs, the second payment output is invalid.
orderFailure3 :: MonadEmulator m => m ()
orderFailure3 = do
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
      , outputs = flip map [spotDatum1, spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
      , outputs = flip map (zip spotUTxOs [(1::Int)..]) $ 
          \((ref, Just SpotDatum{salePrice=Prices salePrice,..}),i) ->
            if even i then
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
            else 
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

-- | When purchasing multiple Spot UTxOs, the first payment output is invalid. There is an unrelated
-- output before the invalid output.
orderFailure4 :: MonadEmulator m => m ()
orderFailure4 = do
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
      , outputs = flip map [spotDatum1, spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
                    [ mconcat $ flip map salePrice $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ PaymentDatum (BeaconId beaconCurrencySymbol,ref)
                , outputReferenceScript = toReferenceScript Nothing
                }
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

-- | When purchasing multiple Spot UTxOs, the second payment output is invalid. There is an unrelated
-- output after the first output but before the invalid output.
orderFailure5 :: MonadEmulator m => m ()
orderFailure5 = do
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
      , outputs = flip map [spotDatum1, spotDatum2] $ \spotDatum@SpotDatum{..} ->
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
      , outputs = mconcat
          [ flip map (take 1 spotUTxOs) $ \(ref, Just SpotDatum{salePrice=Prices salePrice,..}) ->
              Output
                { outputAddress = toCardanoApiAddress paymentAddress
                , outputValue = utxoValue (fromIntegral $ saleDeposit - 1) $ mconcat
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
-- Input Type Failures
-------------------------------------------------
-- | Use PurchaseSpot on a non-Spot UTxO.
inputTypeFailure1 :: MonadEmulator m => m ()
inputTypeFailure1 = do
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

  bidUTxOs <- txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

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
                  toRedeemer PurchaseSpot
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

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for purchasing Spot UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1"
      "Beacon script not executed with proper redeemer"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2"
      "Beacons must go to a DApp address using a valid staking credential"
      beaconFailure2
  , scriptMustFail "beaconFailure3"
      -- Using the BurnBeacons redeemer with a staking execution does not have a user friendly
      -- message.
      beaconFailure3
  , scriptMustFailWithError "beaconFailure4"
      "The wrong beacons were burned"
      beaconFailure4
  , scriptMustFailWithError "beaconFailure5"
      "Beacons not stored with a valid Market UTxO"
      beaconFailure5
  , scriptMustFailWithError "beaconFailure6"
      "This redeemer can only be used to burn beacons"
      beaconFailure6
  , scriptMustFailWithError "beaconFailure7"
      "The wrong beacons were burned"
      beaconFailure7
  , scriptMustFailWithError "beaconFailure8"
      "Invalid Spot UTxO found"
      beaconFailure8
    
    -- Payment Value Failures
  , scriptMustFailWithError "paymentValueFailure1"
      "Spot payment UTxO has wrong value"
      paymentValueFailure1
  , scriptMustFailWithError "paymentValueFailure2"
      "Spot payment UTxO has wrong value"
      paymentValueFailure2
  , scriptMustFailWithError "paymentValueFailure3"
      "Spot payment UTxO missing sale deposit"
      paymentValueFailure3
  , scriptMustFailWithError "paymentValueFailure4"
      "Spot payment UTxO has wrong value"
      paymentValueFailure4

    -- Payment Datum Failures
  , scriptMustFailWithError "paymentDatumFailure1"
      "Not all required spot payments found"
      paymentDatumFailure1
  , scriptMustFailWithError "paymentDatumFailure2"
      "Not all required spot payments found"
      paymentDatumFailure2
  , scriptMustFailWithError "paymentDatumFailure3"
      "Not all required spot payments found"
      paymentDatumFailure3
  , scriptMustFailWithError "paymentDatumFailure4"
      "Not all required spot payments found"
      paymentDatumFailure4

    -- Payment Address Failures
  , scriptMustFailWithError "paymentAddressFailure1"
      "Not all required spot payments found"
      paymentAddressFailure1

    -- Payment Output Order Failures
  , scriptMustFailWithError "orderFailure1"
      "Not all required spot payments found"
      orderFailure1
  , scriptMustFailWithError "orderFailure2"
      "Spot payment UTxO has wrong value"
      orderFailure2
  , scriptMustFailWithError "orderFailure3"
      "Spot payment UTxO has wrong value"
      orderFailure3
  , scriptMustFailWithError "orderFailure4"
      "Spot payment UTxO has wrong value"
      orderFailure4
  , scriptMustFailWithError "orderFailure5"
      "Spot payment UTxO has wrong value"
      orderFailure5

    -- Input Type Failures
  , scriptMustFailWithError "inputTypeFailure1"
      "UTxO is not a Spot UTxO"
      inputTypeFailure1
  ]
