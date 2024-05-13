{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.AuctionUTxOs.UpdateAuctionUTxOs.Benchmarks where

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
-- Benchmarks
-------------------------------------------------
-- | Update multiple valid Auction UTxOs, each one for different nfts. The Auction UTxOs each have 
-- three NFTs for sale and thee assets for the sale price.
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

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      datums = 
        flip map (grouped 3 nfts) $ \xs -> unsafeCreateAuctionDatum $ NewAuctionInfo
          { nftPolicyId = testTokenSymbol
          , nftNames = sort xs
          , startingPrice = 
              [ (Asset (adaSymbol,adaToken), 20_000_000) 
              , (Asset (testTokenSymbol,"TestToken1"), 20) 
              , (Asset (testTokenSymbol,"TestToken2"), 20) 
              ]
          }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1

  -- Try to create the Auction UTxO.
  forM_ (grouped 3 datums) $ \auctionDatums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap auctionDatums $ \AuctionDatum{..} ->
                    [ ("Auction", 1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
            ]
        , outputs = flip map auctionDatums $ \datum@AuctionDatum{..} ->
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

  auctionUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to update the Auction UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map auctionUTxOs $ \(auctionRef,_) ->
          Input
            { inputId = auctionRef
            , inputWitness = 
                SpendWithPlutusReference aftermarketRef InlineDatum $ 
                  toRedeemer CloseOrUpdateSellerUTxO
            }
      , outputs = flip map auctionUTxOs $ \(_,Just datum@AuctionDatum{..}) ->
          Output
            { outputAddress = aftermarketAddress
            , outputValue = utxoValue 5_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol "Auction" 1
                , PV2.singleton beaconCurrencySymbol (unPolicyBeacon $ toPolicyBeacon nftPolicyId) 1
                , mconcat $ flip map nftNames $ \name -> PV2.singleton nftPolicyId name 1
                ]
            , outputDatum = OutputDatum $ toDatum $
                datum & #startingPrice .~ Prices [(Asset (testTokenSymbol,"TestToken2"),20)]
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
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Convert multiple valid Auction UTxOs to Spot UTxOs, each one for different nfts. The Spot UTxOs
-- each have three NFTs for sale and thee assets for the sale price. The sale price includes ada. No
-- beacons need to be changed.
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

      nfts = map (\i -> fromString $ "TestToken" <> show @Int i) [3..123]

      -- Sale Info
      spotDatums = take number $
        flip map (grouped 3 nfts) $ \xs -> unsafeCreateSpotDatum $ NewSpotInfo
          { nftPolicyId = testTokenSymbol
          , nftNames = sort xs
          , paymentAddress = toPlutusAddress sellerPersonalAddr
          , saleDeposit = 5_000_000
          , salePrice = 
              [ (Asset (adaSymbol,adaToken), 20_000_000) 
              , (Asset (testTokenSymbol,"TestToken1"), 20) 
              , (Asset (testTokenSymbol,"TestToken2"), 20) 
              ]
          }

      auctionDatums = flip map (grouped 3 nfts) $ \xs -> unsafeCreateAuctionDatum $ NewAuctionInfo
        { nftPolicyId = testTokenSymbol
        , nftNames = sort xs
        , startingPrice = 
            [ (Asset (adaSymbol,adaToken), 20_000_000) 
            , (Asset (testTokenSymbol,"TestToken1"), 20) 
            , (Asset (testTokenSymbol,"TestToken2"), 20) 
            ]
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1
  mintTestTokens sellerWallet 10_000_000 $ zip nfts $ repeat 1

  -- Try to create the Auction UTxO.
  forM_ (grouped 15 auctionDatums) $ \datums ->
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey]
      emptyTxParams
        { tokens = flip map datums $ \AuctionDatum{..} ->
            TokenMint
              { mintTokens = 
                  [ ("Auction", 1)
                  , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                  ]
              , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just beaconsRef
              }
        , outputs = flip map datums $ \datum@AuctionDatum{..} ->
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

  auctionUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AuctionDatum aftermarketAddress

  -- Try to convert the Spot UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens = mconcat
          [ flip map spotDatums $ \SpotDatum{..} -> 
              TokenMint
                { mintTokens = 
                    [ ("Spot", 1)
                    , (unPolicyBeacon $ toPolicyBeacon nftPolicyId, 1)
                    ]
                , mintRedeemer = toRedeemer CreateCloseOrUpdateMarketUTxOs
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just beaconsRef
                }
          , flip map auctionUTxOs $ \(_, Just AuctionDatum{..}) -> 
              TokenMint
                { mintTokens = 
                    [ ("Auction", -1)
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
      , outputs = flip map spotDatums $ \datum@SpotDatum{..} ->
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
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for updating Auction UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 24
  , mustSucceed "benchTest2" $ benchTest2 22

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 25
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 23
  ]
