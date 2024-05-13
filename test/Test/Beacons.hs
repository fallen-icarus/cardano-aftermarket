module Test.Beacons where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.HUnit

import CardanoAftermarket

-------------------------------------------------
-- Beacon Name Tests
-------------------------------------------------
-- | The BidderId and the policy beacon should have different names, even if the same input
-- is used for both.
nameTest1 :: TestTree
nameTest1 = testCase "nameTest1" $ assertBool "The beacon names are the same" $
    unBidderId (toBidderId $ PV2.PubKeyCredential bidderKey) /= 
      unPolicyBeacon (toPolicyBeacon $ CurrencySymbol cred)
  where
    bidderKey@(PV2.PubKeyHash cred) = 
      LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash $ Mock.knownMockWallet 1
  
-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all beacon uniqueness tests.
tests :: TestTree
tests = testGroup "Beacon Names"
  [ nameTest1
  ]
