{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotBidUTxOs.CloseSpotBidUTxOs.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | Close a single valid Bid UTxO, but withdraw the beacons instead of burning them. The beacon
-- script is not executed.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
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

  -- Try to close the Bid UTxO.
  void $ transact bidderPersonalAddr [aftermarketAddress,refScriptAddress] [bidderPayPrivKey]
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | Close a single valid Bid UTxO, but withdraw the beacons instead of burning them. The beacon
-- script is executed as a staking script.
beaconFailure2 :: MonadEmulator m => m ()
beaconFailure2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
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

  -- Try to close the Bid UTxO.
  void $ transact bidderPersonalAddr [aftermarketAddress,refScriptAddress] [bidderPayPrivKey]
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference beaconsRef $ 
                    toRedeemer CreateCloseOrUpdateMarketUTxOs
              }
          ]
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | Close a single valid Bid UTxO but execute the beacon script with the wrong redeemer.
beaconFailure3 :: MonadEmulator m => m ()
beaconFailure3 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
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

  -- Try to close the Bid UTxO.
  void $ transact bidderPersonalAddr [aftermarketAddress,refScriptAddress] [bidderPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap bidUTxOs $ \(_, Just SpotBidDatum{..}) ->
                  [ ("Bid", -1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                  , (unBidderId $ toBidderId bidderCredential, -1)
                  ]
              , mintRedeemer = toRedeemer BurnBeacons
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | The bidder did not approve closing the valid Bid UTxO.
approvalFailure1 :: MonadEmulator m => m ()
approvalFailure1 = do
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

  -- Try to close the Bid UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap bidUTxOs $ \(_, Just SpotBidDatum{..}) ->
                  [ ("Bid", -1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                  , (unBidderId $ toBidderId bidderCredential, -1)
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
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Address owner did not approve closing an invalid Bid UTxO.
approvalFailure2 :: MonadEmulator m => m ()
approvalFailure2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , outputs = flip map [bidDatum] $ \datum@SpotBidDatum{} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue 5_000_000 $ mconcat [ ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to close the Bid UTxO.
  void $ transact bidderPersonalAddr [aftermarketAddress,refScriptAddress] [bidderPayPrivKey]
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map bidUTxOs $ \(bidRef,_) ->
          Input
            { inputId = bidRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

-------------------------------------------------
-- Datum Failures
-------------------------------------------------
-- | Close a UTxO with a SpotDatum.
datumFailure1 :: MonadEmulator m => m ()
datumFailure1 = do
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

  -- Try to close the Spot UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap spotUTxOs $ \(_, Just SpotDatum{..}) ->
                  [ ("Spot", -1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
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
                  toRedeemer CloseOrUpdateBidderUTxO
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for closing Bid UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1"
      "Beacon script not executed with proper redeemer"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2"
      "Beacons must go to a DApp address using a valid staking credential"
      beaconFailure2
  , scriptMustFailWithError "beaconFailure3"
      "Beacon script not executed with proper redeemer"
      beaconFailure3

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1"
      "Bidder did not approve"
      approvalFailure1
  , scriptMustFailWithError "approvalFailure2"
      "Address owner did not approve"
      approvalFailure2

    -- Datum Failures
  , scriptMustFailWithError "datumFailure1"
      "UTxO is not a Bid UTxO"
      datumFailure1
  ]
