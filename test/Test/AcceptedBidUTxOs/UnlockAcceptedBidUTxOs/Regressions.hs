{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.AcceptedBidUTxOs.UnlockAcceptedBidUTxOs.Regressions where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Basic Regression Tests
-------------------------------------------------
-- | Unlock a single valid AcceptedBid UTxO whose claim period has passed.
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
      { tokens = []
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer $ AcceptClaimBid 0 (toPlutusAddress sellerPersonalAddr)
            }
      , outputs = flip map bidUTxOs $ \(_, Just datum@ClaimBidDatum{..}) ->
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

  awaitTime (slotToPosixTime 3601)
  acceptedBidUTxOs <- txOutRefsAndDatumsAtAddress @AcceptedBidDatum aftermarketAddress
  claimSlot <- currentSlot

  -- Try to unlock the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = flip concatMap acceptedBidUTxOs $ \(_, Just AcceptedBidDatum{..}) ->
                  [ ("Bid", -1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                  , (unBidderId $ toBidderId bidderCredential, -1)
                  ]
              , mintRedeemer = toRedeemer BurnBeacons
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map acceptedBidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer UnlockUnclaimedAcceptedBid
            }
      , outputs = []
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
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim multiple valid AcceptedBid UTxOs, whose claim period has passed. Each bid is for a
-- different bidder credential.
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
      bidderWallet1 = Mock.knownMockWallet 2
      bidderPersonalAddr1 = Mock.mockWalletAddress bidderWallet1
      bidderPayPrivKey1 = Mock.paymentPrivateKey bidderWallet1
      bidderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet1
      bidderCred1 = PV2.PubKeyCredential bidderPubKey1

      bidderWallet2 = Mock.knownMockWallet 3
      bidderPayPrivKey2 = Mock.paymentPrivateKey bidderWallet2
      bidderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet2
      bidderCred2 = PV2.PubKeyCredential bidderPubKey2

      bidderWallet3 = Mock.knownMockWallet 4
      bidderPayPrivKey3 = Mock.paymentPrivateKey bidderWallet3
      bidderPubKey3 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet3
      bidderCred3 = PV2.PubKeyCredential bidderPubKey3

      -- Bid Info
      bidDatum1 = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred1
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum2 = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred2
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum3 = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred3
        , nftNames = 
            [ "TestToken7" 
            , "TestToken8"
            , "TestToken9"
            ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet1 10_000_000 
    [ ("TestToken10",60)
    , ("TestToken11",60)
    ]
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

  -- Try to create the Bid UTxO.
  void $ transact 
    bidderPersonalAddr1 
    [refScriptAddress] 
    [bidderPayPrivKey1,bidderPayPrivKey2,bidderPayPrivKey3]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \ClaimBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \datum@ClaimBidDatum{..} ->
          [ Output
              { outputAddress = aftermarketAddress
              , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                  [ PV2.singleton beaconCurrencySymbol "Bid" 1
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum datum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey1,bidderPubKey2,bidderPubKey3]
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
      { tokens = []
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer $ AcceptClaimBid 0 (toPlutusAddress sellerPersonalAddr)
            }
      , outputs = flip map bidUTxOs $ \(_, Just datum@ClaimBidDatum{..}) ->
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

  awaitTime (slotToPosixTime 3601)
  acceptedBidUTxOs <- txOutRefsAndDatumsAtAddress @AcceptedBidDatum aftermarketAddress
  claimSlot <- currentSlot

  -- Try to unlock the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = flip concatMap acceptedBidUTxOs $ \(_, Just AcceptedBidDatum{..}) ->
                  [ ("Bid", -1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                  , (unBidderId $ toBidderId bidderCredential, -1)
                  ]
              , mintRedeemer = toRedeemer BurnBeacons
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map acceptedBidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer UnlockUnclaimedAcceptedBid
            }
      , outputs = []
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
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Unlock invalid AcceptedBid UTxO (ie, it is missing the beacons). The observer script
-- was not executed.
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
      { tokens = []
      , outputs =
          [ Output
              { outputAddress = aftermarketAddress
              , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat []
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  acceptedBidUTxOs <- txOutRefsAndDatumsAtAddress @AcceptedBidDatum aftermarketAddress

  -- Try to unlock the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = []
      , inputs = flip map acceptedBidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer UnlockUnclaimedAcceptedBid
            }
      , outputs = []
      , referenceInputs = [aftermarketObserverRef,beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all regression scenarios for unlocking Bid UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  ]
