{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.SpotBidUTxOs.UpdateSpotBidUTxOs.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.List (sort)
import Data.String (fromString)
import Control.Monad (forM_)
import Optics.Optic
import Optics.Operators

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Update multiple valid Bid UTxOs. The bid is for three NFTs and uses three assets
-- for the bid. The bid does not use ada. No beacons need to be changed.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
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

      assets = 
        map (\i -> (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..124]

      -- Bid Info
      datums = flip map (grouped 3 assets) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred
        , nftNames = 
            [ "TestToken1"
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr
        , bidDeposit = 5_000_000
        , bid = sort $ zip (map Asset xs) $ repeat 20
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet 10_000_000 $ zip (map snd assets) $ repeat 20

  -- Try to create the Bid UTxO.
  forM_ (grouped 15 datums) $ \bidDatums ->
    transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey]
      emptyTxParams
        { tokens = flip map bidDatums $ \SpotBidDatum{..} ->
            TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
        , outputs = flip map bidDatums $ \bidDatum@SpotBidDatum{bid=Prices bid,..} ->
            Output
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
        , referenceInputs = [beaconsRef]
        , extraKeyWitnesses = [bidderPubKey]
        }

  bidUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to update the Bid UTxO.
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
      , outputs = flip concatMap bidUTxOs $ 
          \(_, Just datum@SpotBidDatum{bid=Prices bid,..}) ->
            [ Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue 6_000_000 $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCredential) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ datum & #bidDeposit .~ 6_000_000
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
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

-- | Change the bidder credential for multiple valid Bid UTxOs. The bid is for three NFTs and 
-- uses three assets for the bid. The bid does not use ada. No beacons need to be changed.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
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

      assets = 
        map (\i -> (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..124]

      -- Bid Info
      datums = flip map (grouped 3 assets) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
        { nftPolicyId = testTokenSymbol
        , bidderCredential = bidderCred1
        , nftNames = 
            [ "TestToken1"
            , "TestToken2"
            , "TestToken3"
            ]
        , paymentAddress = toPlutusAddress bidderPersonalAddr1
        , bidDeposit = 5_000_000
        , bid = sort $ zip (map Asset xs) $ repeat 20
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens bidderWallet1 10_000_000 $ zip (map snd assets) $ repeat 20

  -- Try to create the Bid UTxO.
  forM_ (grouped 15 datums) $ \bidDatums ->
    transact bidderPersonalAddr1 [refScriptAddress] [bidderPayPrivKey1]
      emptyTxParams
        { tokens = flip map bidDatums $ \SpotBidDatum{..} ->
            TokenMint
              { mintTokens = 
                  [ ("Bid", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  , (unBidderId $ toBidderId bidderCredential, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
        , outputs = flip map bidDatums $ \bidDatum@SpotBidDatum{bid=Prices bid,..} ->
            Output
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
        , referenceInputs = [beaconsRef]
        , extraKeyWitnesses = [bidderPubKey1]
        }

  bidUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @SpotBidDatum aftermarketAddress

  -- Try to update the Bid UTxO.
  void $ transact 
    bidderPersonalAddr1
    [aftermarketAddress,refScriptAddress] 
    [bidderPayPrivKey1,bidderPayPrivKey2]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = flip concatMap bidUTxOs $ \(_, Just SpotBidDatum{..}) ->
                  [ (unBidderId $ toBidderId bidderCredential, -1)
                  , (unBidderId $ toBidderId bidderCred2, 1)
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
      , outputs = flip concatMap bidUTxOs $ 
          \(_, Just datum@SpotBidDatum{bid=Prices bid,..}) ->
            [ Output
                { outputAddress = aftermarketAddress
                , outputValue = utxoValue (fromIntegral bidDeposit) $ mconcat
                    [ PV2.singleton beaconCurrencySymbol "Bid" 1
                    , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                    , PV2.singleton beaconCurrencySymbol (unBidderId $ toBidderId bidderCred2) 1
                    , mconcat $ flip map bid $ \(Asset (sym,name), num) -> 
                        PV2.singleton sym name num
                    ]
                , outputDatum = OutputDatum $ toDatum $ datum & #bidderCredential .~ bidderCred2
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [bidderPubKey1,bidderPubKey2]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for updating Bid UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 14
  , mustSucceed "benchTest2" $ benchTest2 14

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 15
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 15
  ]
