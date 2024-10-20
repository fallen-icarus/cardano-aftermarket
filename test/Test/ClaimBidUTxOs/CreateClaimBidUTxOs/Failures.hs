module Test.ClaimBidUTxOs.CreateClaimBidUTxOs.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Optics.Operators
import Optics.Optic

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | The "Bid" beacon has a different name.
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bd", 1)
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
                  [ PV2.singleton beaconCurrencySymbol "Bd" 1
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The BidderId has a different name.
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , ("other", 1)
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
                  , PV2.singleton beaconCurrencySymbol "other" 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The Policy beacon has a different name.
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , ("other", 1)
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
                  , PV2.singleton beaconCurrencySymbol "other" 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The "Bid" beacon is missing.
beaconFailure4 :: MonadEmulator m => m ()
beaconFailure4 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 0)
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
                  [ PV2.singleton beaconCurrencySymbol "Bid" 0
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The BidderId is missing.
beaconFailure5 :: MonadEmulator m => m ()
beaconFailure5 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 0)
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
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 0
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The Policy beacon is missing.
beaconFailure6 :: MonadEmulator m => m ()
beaconFailure6 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 0)
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
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 0
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | There are too many "Bid" beacons in the Bid UTxO.
beaconFailure7 :: MonadEmulator m => m ()
beaconFailure7 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 2)
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
                  [ PV2.singleton beaconCurrencySymbol "Bid" 2
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | There are too many Policy beacons in the Bid UTxO.
beaconFailure8 :: MonadEmulator m => m ()
beaconFailure8 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 2)
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
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 2
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | There are too many BidderIds in the Bid UTxO.
beaconFailure9 :: MonadEmulator m => m ()
beaconFailure9 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 2)
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
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 2
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-------------------------------------------------
-- Value Failures
-------------------------------------------------
-- | The bid is present. Ada is part of the bid.
valueFailure1 :: MonadEmulator m => m ()
valueFailure1 = do
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
      bidDatum@ClaimBidDatum{bid=Prices bid,..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
                  , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                      PV2.singleton sym name num
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The bid is present. Ada is not part of the bid.
valueFailure2 :: MonadEmulator m => m ()
valueFailure2 = do
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
      bidDatum@ClaimBidDatum{bid=Prices bid,..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (testTokenSymbol,"TestToken10"), 20) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 
    [ ("TestToken10",20) ]

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
                  , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                      PV2.singleton sym name num
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The UTxO is missing the bid deposit. The bid uses ada.
valueFailure3 :: MonadEmulator m => m ()
valueFailure3 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              , outputValue = utxoValue 3_000_000 $ mconcat
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The UTxO is missing the bid deposit. The bid does not use ada.
valueFailure4 :: MonadEmulator m => m ()
valueFailure4 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (testTokenSymbol,"TestToken10"), 20) ]
        }

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
              , outputValue = utxoValue 3_000_000 $ mconcat
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-------------------------------------------------
-- Datum Failures
-------------------------------------------------
-- | The UTxO does not have a ClaimBidDatum.
datumFailure1 :: MonadEmulator m => m ()
datumFailure1 = do
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
      bidDatum@SpotDatum{..} = unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , paymentAddress = toPlutusAddress aftermarketAddress
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

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
                  , (unBidderId $ toBidderId bidderCred, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = aftermarketAddress
              , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                  [ PV2.singleton beaconCurrencySymbol "Bid" 1
                  , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                  , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCred) 1
                  ]
              , outputDatum = OutputDatum $ toDatum bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Bid UTxO has a datum hash.
datumFailure2 :: MonadEmulator m => m ()
datumFailure2 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              , outputDatum = OutputDatumHash $ datumHash bidDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Bid UTxO has no datum.
datumFailure3 :: MonadEmulator m => m ()
datumFailure3 = do
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
      ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Datum has wrong beacon id.
datumFailure4 :: MonadEmulator m => m ()
datumFailure4 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              , outputDatum = OutputDatum $ toDatum $
                  bidDatum & #beaconId .~ BeaconId ""
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Datum has wrong aftermarket observer hash.
datumFailure5 :: MonadEmulator m => m ()
datumFailure5 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              , outputDatum = OutputDatum $ toDatum $
                  bidDatum & #aftermarketObserverHash .~ ""
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | NFT policy id is the beaocn policy.
datumFailure6 :: MonadEmulator m => m ()
datumFailure6 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = beaconCurrencySymbol
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The bid is empty.
datumFailure7 :: MonadEmulator m => m ()
datumFailure7 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = []
        }

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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The NFT names have duplicates.
datumFailure8 :: MonadEmulator m => m ()
datumFailure8 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1"
            , "TestToken1"
            ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The NFT names are not sorted.
datumFailure9 :: MonadEmulator m => m ()
datumFailure9 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken2"
            , "TestToken1"
            ]

        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Bid has a negative value.
datumFailure10 :: MonadEmulator m => m ()
datumFailure10 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), -1) ]
        }

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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Bid has a zero value.
datumFailure11 :: MonadEmulator m => m ()
datumFailure11 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 0) ]
        }

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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | NFT names is empty.
datumFailure12 :: MonadEmulator m => m ()
datumFailure12 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = []
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Datum has a negative deposit.
datumFailure13 :: MonadEmulator m => m ()
datumFailure13 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = -5_000_000
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
              , outputValue = utxoValue 3_000_000 $ mconcat
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Datum has a zero deposit.
datumFailure14 :: MonadEmulator m => m ()
datumFailure14 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 0
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | A protocol beacon is part of the bid. The beacon is another "Bid" beacon.
datumFailure15 :: MonadEmulator m => m ()
datumFailure15 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000)
            , (Asset (beaconCurrencySymbol,"Bid"), 1)
            ]
        }

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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | A protocol beacon is part of the bid. The beacon is an unrelated beacon.
datumFailure16 :: MonadEmulator m => m ()
datumFailure16 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000)
            , (Asset (beaconCurrencySymbol,"Other"), 1)
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , ("Other", 1)
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The bid is not sorted.
datumFailure17 :: MonadEmulator m => m ()
datumFailure17 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (testTokenSymbol,"TestToken2"), 1)
            , (Asset (adaSymbol,adaToken), 20_000_000)
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 [("TestToken2",1)]

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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-------------------------------------------------
-- Destination Failures
-------------------------------------------------
-- | The Bid UTxO is sent to a non-dapp address.
destinationFailure1 :: MonadEmulator m => m ()
destinationFailure1 = do
  let -- Seller Info
      -- sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      -- sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      -- sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      -- sellerCred = PV2.PubKeyCredential sellerPubKey
      -- aftermarketAddress = toCardanoApiAddress $ PV2.Address 
      --   { addressCredential = PV2.ScriptCredential aftermarketScriptHash
      --   , addressStakingCredential = Just $ PV2.StakingHash sellerCred
      --   }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      -- Bid Info
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
              { outputAddress = bidderPersonalAddr
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | The Bid UTxO is sent to a dapp address without staking.
destinationFailure2 :: MonadEmulator m => m ()
destinationFailure2 = do
  let -- Seller Info
      -- sellerWallet = Mock.knownMockWallet 1
      -- sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      -- sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      -- sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      -- sellerCred = PV2.PubKeyCredential sellerPubKey
      aftermarketAddress = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Nothing
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      -- Bid Info
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | The bidder credential did not approve the transaction.
approvalFailure1 :: MonadEmulator m => m ()
approvalFailure1 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidDeposit = 5_000_000
        , bidExpiration = Nothing
        , claimExpiration = slotToPosixTime 3600
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-- | Not all required bidder credential's approved the transaction.
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
      -- bidderPayPrivKey3 = Mock.paymentPrivateKey bidderWallet3
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
    [bidderPayPrivKey1,bidderPayPrivKey2]
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
      , extraKeyWitnesses = [bidderPubKey1,bidderPubKey2]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 3600
          }
      }

-------------------------------------------------
-- Time Failures
-------------------------------------------------
-- | Invalid-hereafter is not specified.
timeFailure1 :: MonadEmulator m => m ()
timeFailure1 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-- | The bid does not expire but invalid hereafter is after the claimExpiration.
timeFailure2 :: MonadEmulator m => m ()
timeFailure2 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 4000
          }
      }

-- | The bid expires but invalid hereafter is after the bid expiration time.
timeFailure3 :: MonadEmulator m => m ()
timeFailure3 = do
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
      bidDatum@ClaimBidDatum{..} = unsafeCreateClaimBidDatum $ NewClaimBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" ]
        , bidExpiration = Just 1000
        , claimExpiration = slotToPosixTime 3600
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
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just 2000
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for creating ClaimBid UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1"
      "Bid UTxO has wrong value"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2"
      "Bid UTxO has wrong value"
      beaconFailure2
  , scriptMustFailWithError "beaconFailure3"
      "Bid UTxO has wrong value"
      beaconFailure3
  , scriptMustFailWithError "beaconFailure4"
      "Beacons not stored with a valid Market UTxO"
      beaconFailure4
  , scriptMustFailWithError "beaconFailure5"
      "Beacons not stored with a valid Market UTxO"
      beaconFailure5
  , scriptMustFailWithError "beaconFailure6"
      "Beacons not stored with a valid Market UTxO"
      beaconFailure6
  , scriptMustFailWithError "beaconFailure7"
      "Bid UTxO has wrong value"
      beaconFailure7
  , scriptMustFailWithError "beaconFailure8"
      "Bid UTxO has wrong value"
      beaconFailure8
  , scriptMustFailWithError "beaconFailure9"
      "Bid UTxO has wrong value"
      beaconFailure9

    -- Value Failures
  , scriptMustFailWithError "valueFailure1"
      "Bid UTxO missing bid deposit"
      valueFailure1
  , scriptMustFailWithError "valueFailure2"
      "Bid UTxO has wrong value"
      valueFailure2
  , scriptMustFailWithError "valueFailure3"
      "Bid UTxO missing bid deposit"
      valueFailure3
  , scriptMustFailWithError "valueFailure4"
      "Bid UTxO missing bid deposit"
      valueFailure4

    -- Datum Failures
  , scriptMustFailWithError "datumFailure1"
      "Beacons stored with invalid datum"
      datumFailure1
  , scriptMustFailWithError "datumFailure2"
      "All datums must be inline datums"
      datumFailure2
  , scriptMustFailWithError "datumFailure3"
      "All datums must be inline datums"
      datumFailure3
  , scriptMustFailWithError "datumFailure4"
      "Datum has wrong beacon id"
      datumFailure4
  , scriptMustFailWithError "datumFailure5"
      "Datum has wrong aftermarket observer hash"
      datumFailure5
  , scriptMustFailWithError "datumFailure6"
      "NFTs cannot be protocol beacons"
      datumFailure6
  , scriptMustFailWithError "datumFailure7"
      "Bid cannot be empty"
      datumFailure7
  , scriptMustFailWithError "datumFailure8"
      "Duplicate asset found"
      datumFailure8
  , scriptMustFailWithError "datumFailure9"
      "NFT names must be sorted"
      datumFailure9
  , scriptMustFailWithError "datumFailure10"
      "All prices must be > 0"
      datumFailure10
  , scriptMustFailWithError "datumFailure11"
      "All prices must be > 0"
      datumFailure11
  , scriptMustFailWithError "datumFailure12"
      "NFT names cannot be empty"
      datumFailure12
  , scriptMustFailWithError "datumFailure13"
      "Invalid bid deposit"
      datumFailure13
  , scriptMustFailWithError "datumFailure14"
      "Invalid bid deposit"
      datumFailure14
  , scriptMustFailWithError "datumFailure15"
      "Protocol beacons cannot be bid"
      datumFailure15
  , scriptMustFailWithError "datumFailure16"
      "Beacons not stored with a valid Market UTxO"
      datumFailure16
  , scriptMustFailWithError "datumFailure17"
      "Bid assets must be sorted"
      datumFailure17

    -- Destination Failures
  , scriptMustFailWithError "destinationFailure1"
      "Beacons must go to a DApp address using a valid staking credential"
      destinationFailure1
  , scriptMustFailWithError "destinationFailure2"
      "Beacons must go to a DApp address using a valid staking credential"
      destinationFailure2

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1"
      "Bidder credential did not approve"
      approvalFailure1
  , scriptMustFailWithError "approvalFailure2"
      "Bidder credential did not approve"
      approvalFailure2

    -- Time Failures
  , scriptMustFailWithError "timeFailure1"
      "invalid-hereafter not specified"
      timeFailure1
  , scriptMustFailWithError "timeFailure2"
      "Invalid claim expiration"
      timeFailure2
  , scriptMustFailWithError "timeFailure3"
      "Invalid bid expiration"
      timeFailure3
  ]
