{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotUTxOs.PurchaseSpotUTxOs.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)
import Data.List (sort)
import Control.Monad (forM_)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Benchmarks
-------------------------------------------------
-- | Purchase multiple valid Spot UTxOs, each one for different nfts. The Spot UTxOs each have 
-- three NFTs for sale and thee assets for the sale price. The sale price includes ada. BurnBeacons
-- is used to burn the beacons.
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      datums = flip map (grouped 3 nfts) $ \xs -> unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = sort xs
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 10_000_000) 
            , (Asset (testTokenSymbol,"TestToken1"), 20) 
            , (Asset (testTokenSymbol,"TestToken2"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1
  mintTestTokens buyerWallet (fromIntegral $ 10_000_000 * number)
    [ ("TestToken1", fromIntegral $ number * 20)
    , ("TestToken2", fromIntegral $ number * 20)
    ]

  -- Try to create the Spot UTxO.
  forM_ (grouped 15 datums) $ \spotDatums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey]
      emptyTxParams
        { tokens = flip map spotDatums $ \SpotDatum{..} ->
            TokenMint
              { mintTokens = 
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
        , outputs = flip map spotDatums $ \spotDatum@SpotDatum{..} ->
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

  spotUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

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
-- three NFTs for sale and thee assets for the sale price. The sale price includes ada.
-- CreateCloseOrUpdateMarketUTxOs is used to burn the beacons.
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      -- buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      -- buyerCred = PV2.PubKeyCredential buyerPubKey

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      datums = flip map (grouped 3 nfts) $ \xs -> unsafeCreateSpotDatum $ NewSpotInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = sort xs
        , paymentAddress = toPlutusAddress sellerPersonalAddr
        , saleDeposit = 5_000_000
        , salePrice = 
            [ (Asset (adaSymbol,adaToken), 10_000_000) 
            , (Asset (testTokenSymbol,"TestToken1"), 20) 
            , (Asset (testTokenSymbol,"TestToken2"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1
  mintTestTokens buyerWallet (fromIntegral $ 10_000_000 * number)
    [ ("TestToken1", fromIntegral $ number * 20)
    , ("TestToken2", fromIntegral $ number * 20)
    ]

  -- Try to create the Spot UTxO.
  forM_ (grouped 15 datums) $ \spotDatums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey]
      emptyTxParams
        { tokens = flip map spotDatums $ \SpotDatum{..} ->
            TokenMint
              { mintTokens = 
                  [ ("Spot", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
        , outputs = flip map spotDatums $ \spotDatum@SpotDatum{..} ->
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

  spotUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotDatum aftermarketAddress

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

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for purchasing Spot UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 22
  , mustSucceed "benchTest2" $ benchTest2 21

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 23
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 22
  ]
