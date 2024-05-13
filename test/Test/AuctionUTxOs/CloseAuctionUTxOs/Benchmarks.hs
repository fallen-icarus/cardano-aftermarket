{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.AuctionUTxOs.CloseAuctionUTxOs.Benchmarks where

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
-- | Close multiple valid Auction UTxOs, each one for different nfts. The Auction UTxOs each have 
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

  -- Try to close the Auction UTxO.
  void $ transact sellerPersonalAddr [aftermarketAddress,refScriptAddress] [sellerPayPrivKey]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap auctionUTxOs $ \(_, Just AuctionDatum{..}) ->
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
      , referenceInputs = [beaconsRef,aftermarketRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for close Auction UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 32

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 33
  ]
