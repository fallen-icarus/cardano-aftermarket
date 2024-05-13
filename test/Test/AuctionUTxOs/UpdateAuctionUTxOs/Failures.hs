{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.AuctionUTxOs.UpdateAuctionUTxOs.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | When converting beacons, withdraw the old beacons.
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

      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (testTokenSymbol,"TestToken2"), 20) ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1)]

  -- Try to create the Auction UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = flip map [auctionDatum] $ \AuctionDatum{..} -> 
          TokenMint
            { mintTokens = 
                [ ("Auction", 1)
                , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                ]
            , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
            , mintPolicy = toVersionedMintingPolicy beaconScript
            , mintReference = Just beaconsRef
            }
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

  auctionUTxOs <- txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to update the Auction UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = mconcat
          [ flip map [spotDatum] $ \SpotDatum{..} -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", 1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          , flip map [auctionDatum] $ \AuctionDatum{..} -> 
              TokenMint
                { mintTokens = 
                    [ ("Auction", 0)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, -1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          ]
      , inputs = flip map auctionUTxOs $ \(auctionRef,_) ->
          Input
            { inputId = auctionRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateSellerUTxO
            }
      , outputs = flip map [spotDatum] $ \datum@SpotDatum{..} ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue (fromIntegral saleDeposit) $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Spot" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | The seller did not approve the update.
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

      -- Other User Info
      otherWallet = Mock.knownMockWallet 2
      otherPersonalAddr = Mock.mockWalletAddress otherWallet
      otherPayPrivKey = Mock.paymentPrivateKey otherWallet
      otherPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash otherWallet

      -- Sale Info
      auctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) ]
        }

      newAuctionDatum = unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = 
            [ "TestToken1" ]
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 10_000_000) ]
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

  auctionUTxOs <- txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to update the Auction UTxO.
  void $ transact otherPersonalAddr [aftermarketAddress,refScriptAddress] [otherPayPrivKey]
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map auctionUTxOs $ \(auctionRef,_) ->
          Input
            { inputId = auctionRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateSellerUTxO
            }
      , outputs = flip map [newAuctionDatum] $ \datum@AuctionDatum{..} ->
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
      , extraKeyWitnesses = [otherPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for updating Auction UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1"
      "Beacons not stored with a valid Market UTxO"
      beaconFailure1

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1"
      "Seller did not approve"
      approvalFailure1
  ]
