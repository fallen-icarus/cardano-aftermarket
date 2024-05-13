use aiken/transaction/value.{PolicyId}

pub type MarketRedeemer {
  // Close or update either a Spot UTxO or an Auction UTxO.
  CloseOrUpdateSellerUTxO

  // Close or update a Bid UTxO.
  CloseOrUpdateBidderUTxO

  // Purchase a Spot UTxO.
  PurchaseSpot

  // Accept a Bid UTxO.
  AcceptBid
}

pub type BeaconsRedeemer {
  // Create, close, or update some Market UTxOs (1 or more). 
  CreateCloseOrUpdateMarketUTxOs

  // Burn any beacons. This can only be used when the payment observer is executed.
  BurnBeacons

  // Register the script.
  RegisterBeaconsScript
}

// Redeemers for the payment observer script.
pub type PaymentObserverRedeemer {
  // Observe a market payment transaction. This observes the payment for both spot purchases and
  // bid acceptances.
  ObservePayment{ beacon_policy_id: PolicyId }

  // Register the payment observer script allow it to be used as a staking script.
  RegisterPaymentObserverScript
}
