module Test.SpotBidUTxOs.CreateSpotBidUTxOs.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)
import Data.List (sort)

import CardanoAftermarket

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple valid Bid UTxOs. The bid is for three NFTs and uses three assets
-- for the bid. The bid does not use ada.
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

      assets = take (number*3) $ 
        map (\i -> (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..]

      -- Bid Info
      bidDatums = flip map (grouped 3 assets) $ \xs -> unsafeCreateSpotBidDatum $ NewSpotBidInfo
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
  void $ transact bidderPersonalAddr [refScriptAddress] [bidderPayPrivKey] $
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

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for creating SpotBid UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 20

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 21
  ]
