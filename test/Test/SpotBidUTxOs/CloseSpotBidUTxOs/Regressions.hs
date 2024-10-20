{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotBidUTxOs.CloseSpotBidUTxOs.Regressions where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Basic Regression Tests
-------------------------------------------------
-- | Close a single valid Bid UTxO. The bid is for only one NFT and only one asset 
-- is used for the bid. The bid uses ada.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
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
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | Close multiple valid Bid UTxOs, each one for different nfts. The Bid UTxOs each have 
-- three NFTs for sale and thee assets for the bid. The bid includes ada. All bids are for the same
-- bidder credential.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
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
      bidDatum1 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum2 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum3 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken7" 
            , "TestToken8"
            , "TestToken9"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 
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
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
          [ Output
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
          ]
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
      , extraKeyWitnesses = [bidderPubKey]
      }

-- | Close a single invalid Bid UTxO.
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
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
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
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Close multiple valid Bid UTxOs, each one for different nfts. The Bid UTxOs each have 
-- three NFTs for sale and thee assets for the bid. The bid includes ada. Each bid is for a
-- different bidder credential. All credentials approved.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
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
      bidderPayPrivKey3 = Mock.paymentPrivateKey bidderWallet3
      bidderPubKey3 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet3
      bidderCred3 = PV2.PubKeyCredential bidderPubKey3

      -- Bid Info
      bidDatum1 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred1
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr1
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum2 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred2
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr1
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum3 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred3
        , nftNames = 
            [ "TestToken7" 
            , "TestToken8"
            , "TestToken9"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr1
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
              { mintTokens = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = flip concatMap [bidDatum1,bidDatum2,bidDatum3] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
          [ Output
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
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey1,bidderPubKey2,bidderPubKey3]
      }

  bidUTxOs <- txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to close the Bid UTxO.
  void $ 
    transact 
    bidderPersonalAddr1
    [aftermarketAddress,refScriptAddress] 
    [bidderPayPrivKey1,bidderPayPrivKey2,bidderPayPrivKey3]
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
      , extraKeyWitnesses = [bidderPubKey1,bidderPubKey2,bidderPubKey3]
      }

-- | Close multiple valid Bid UTxOs that are located at different addresses. 
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let -- Seller1 Info
      sellerWallet1 = Mock.knownMockWallet 1
      -- sellerPersonalAddr1 = Mock.mockWalletAddress sellerWallet1
      -- sellerPayPrivKey1 = Mock.paymentPrivateKey sellerWallet1
      sellerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet1
      sellerCred1 = PV2.PubKeyCredential sellerPubKey1
      aftermarketAddress1 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred1
        }

      -- Seller2 Info
      sellerWallet2 = Mock.knownMockWallet 2
      -- sellerPersonalAddr2 = Mock.mockWalletAddress sellerWallet2
      -- sellerPayPrivKey2 = Mock.paymentPrivateKey sellerWallet2
      sellerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet2
      sellerCred2 = PV2.PubKeyCredential sellerPubKey2
      aftermarketAddress2 = toCardanoApiAddress $ PV2.Address 
        { addressCredential = PV2.ScriptCredential aftermarketScriptHash
        , addressStakingCredential = Just $ PV2.StakingHash sellerCred2
        }

      -- Bidder Info
      bidderWallet = Mock.knownMockWallet 2
      bidderPersonalAddr = Mock.mockWalletAddress bidderWallet
      bidderPayPrivKey = Mock.paymentPrivateKey bidderWallet
      bidderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash bidderWallet
      bidderCred = PV2.PubKeyCredential bidderPubKey

      -- Bid Info
      bidDatum1 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1" 
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }
      bidDatum2 = unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken4" 
            , "TestToken5"
            , "TestToken6"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken10"), 20) 
            , (Asset (testTokenSymbol,"TestToken11"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 
    [ ("TestToken10",60)
    , ("TestToken11",60)
    ]
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

  -- Try to create the Bid UTxO.
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap [bidDatum1,bidDatum2] $ \SpotBidDatum{..} ->
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
          ]
      , outputs = mconcat
          [ flip map [bidDatum1] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
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
          , flip map [bidDatum2] $ \datum@SpotBidDatum{bid=Prices bid,..} ->
              Output
                { outputAddress = aftermarketAddress2
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
          ]
      , referenceInputs = [beaconsRef]
      , extraKeyWitnesses = [bidderPubKey]
      }

  bidUTxOs <- (<>) <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress1
                   <*> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress2

  -- Try to close the Bid UTxO.
  void $ 
    transact 
    bidderPersonalAddr
    [aftermarketAddress1,aftermarketAddress2,refScriptAddress] 
    [bidderPayPrivKey]
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
      , extraKeyWitnesses = [bidderPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all regression scenarios for closing Bid UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5
  ]
