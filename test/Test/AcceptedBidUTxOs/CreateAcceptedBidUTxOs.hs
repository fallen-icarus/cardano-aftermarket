module Test.AcceptedBidUTxOs.CreateAcceptedBidUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AcceptedBidUTxOs.CreateAcceptedBidUTxOs.Failures qualified as Failures

-- It should never be possible to create them except through accepting claim bid UTxOs.
tests :: TestTree
tests = testGroup "Creating AcceptedBid UTxOs" $ mconcat
  [ Failures.tests
  ]
