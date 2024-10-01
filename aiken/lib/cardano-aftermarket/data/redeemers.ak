use aiken/transaction/value.{PolicyId}
use aiken/transaction/credential.{Address}

pub type MarketRedeemer {
  // Close or update either a Spot UTxO or an Auction UTxO.
  CloseOrUpdateSellerUTxO

  // Close or update a Bid UTxO.
  CloseOrUpdateBidderUTxO

  // Purchase a Spot UTxO.
  PurchaseSpot

  // Accept a Spot Bid UTxO.
  AcceptSpotBid
  
  // Accept claimable Bid UTxO. The seller_deposit will be stored with the AcceptedBid UTxO and the
  // payment_address is the address the seller wishes to have the bid amount sent to.
  AcceptClaimBid { seller_deposit: Int, payment_address: Address }

  // Claim accepted Bid UTxO.
  ClaimAcceptedBid

  // Unlock an unclaimed accepted bid after the claim expiration has passed.
  UnlockUnclaimedAcceptedBid
}

pub type BeaconsRedeemer {
  // Create, close, or update some Market UTxOs (1 or more). 
  CreateCloseOrUpdateMarketUTxOs

  // Burn any beacons. This can only be used when the payment observer is executed.
  BurnBeacons

  // Register the script.
  RegisterBeaconsScript
}

// Redeemers for the aftermarket observer script.
pub type MarketObserverRedeemer {
  // Observe a market payment or acceptance transaction. 
  ObserveAftermarket{ beacon_policy_id: PolicyId }

  // Register the aftermarket observer script allow it to be used as a staking script.
  RegisterAftermarketObserverScript
}
