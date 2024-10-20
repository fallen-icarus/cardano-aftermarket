use aiken/transaction/credential.{Address, Credential}
use aiken/time.{PosixTime}
use aiken/transaction/value.{PolicyId, AssetName}

use cardano_aftermarket/data/core.{Asset, ScriptHash}

pub type MarketDatum {
  // A datum for a sale ready for immediate fulfillment by a bidder. It will be stored with a "Spot"
  // beacon and the policy beacon for the NFTs being sold. It must be stored with the actual NFTs
  // being sold.
  SpotDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The aftermarket observer script hash.
    aftermarket_observer_hash: ScriptHash,
    // The policy id for the NFTs being sold.
    nft_policy_id: PolicyId,
    // The token names of the NFTs being sold. The names must be sorted.
    nft_names: List<AssetName>,
    // The address where the payment output must go.
    payment_address: Address,
    // The amount the seller paid for the minUTxOValue. This must be returned in the payment output.
    sale_deposit: Int,
    // The price for this batch, denominated in the specified assets. This is the price for the
    // entire batch of NFTs. The payment output must contain *all* of the required assets.
    sale_price: List<(Asset,Int)>
  }

  // A datum for an auction looking for bids. It will be stored with an "Auction" beacon and the
  // policy beacon for the NFTs being auctioned. It must be stored with the actual NFTs being
  // auctioned to prove the auctioner actually controls them.
  AuctionDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The aftermarket observer script hash.
    aftermarket_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The token names for the NFTs being auctioned. The names must be sorted.
    nft_names: List<AssetName>,
    // The desired starting price. This is only used to broadcast the auctioner's desired value.
    // Bidders are able to create "counter-bids" with different assets and/or different nft names.
    // For example, the bidder can make a bid for just one of the NFTs in the batch.
    starting_price: List<(Asset,Int)>
  }

  // A datum for a bid for an auction where the seller should immediately send the asset to the
  // bidder. It will be stored with a "Bid" beacon and the policy beacon for the NFTs being bid on.
  // It must be stored with the bid amount.
  SpotBidDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The aftermarket observer script hash.
    aftermarket_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The credential used for the BidderId token name. This is used so bidders can find all of
    // their bids despite them being located in other addresses. The BidderId is the hash of their
    // staking credential.
    bidder_credential: Credential,
    // The token names of the NFTs the bid is for. This does not have to be the same as the list in
    // the corresponding Auction UTxO. The names must be sorted.
    nft_names: List<AssetName>,
    // The address where the payment output must go if the bid is accepted. It must contain the NFTs
    // + the bid_deposit.
    payment_address: Address,
    // The amount the bidder paid for the minUTxOValue. This will be returned in the payment output.
    bid_deposit: Int,
    // The size of the bid.
    bid: List<(Asset,Int)>
  }

  // A datum for a bid for an auction where the seller should lock up the NFT and wait for the
  // bidder to come claim it. This enables the bidder to back out if needed. This feature is useful
  // for the loan aftermarket where the bidder needs to update the lender payment address upon
  // purchase. If the borrower fully pays off the loan before the bidder can claim it, the bidder
  // should not be forced to claim it. The seller can claim the bidder's deposit in the case where
  // they back out. It will be stored with a "Bid" beacon and the policy beacon for the NFTs being
  // bid on. It does not need to be stored with the bid amount.
  ClaimBidDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The aftermarket observer script hash.
    aftermarket_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The credential used for the BidderId token name. This is used so bidders can find all of
    // their bids despite them being located in other addresses. The BidderId is the hash of their
    // staking credential.
    bidder_credential: Credential,
    // The token names of the NFTs the bid is for. This does not have to be the same as the list in
    // the corresponding Auction UTxO. The names must be sorted.
    nft_names: List<AssetName>,
    // The amount the bidder paid for the minUTxOValue.
    bid_deposit: Int,
    // The size of the bid.
    bid: List<(Asset,Int)>,
    // The time the bid expires.
    bid_expiration: Option<PosixTime>,
    // The time the bidder is saying they can claim the NFTs by.
    claim_expiration: PosixTime
  }

  // A datum for a bid for an auction where the seller accepted the `ClaimBidDatum`. It is waiting
  // for the bidder to come claim it. The bidder can still back out. If the bidder backs out, the
  // seller can claim the bidders minUTxOValue deposit. It will be stored with a "Bid" beacon
  // and the policy beacon for the NFTs being auctioned. It needs to be stored with the actual NFTs.
  AcceptedBidDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The aftermarket observer script hash.
    aftermarket_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The credential used for the BidderId token name. This is used so bidders can find all of
    // their bids despite them being located in other addresses. The BidderId is the hash of their
    // staking credential.
    bidder_credential: Credential,
    // The token names of the NFTs the bid is for. This does not have to be the same as the list in
    // the corresponding Auction UTxO. The names must be sorted.
    nft_names: List<AssetName>,
    // The amount the bidder paid for the minUTxOValue.
    bid_deposit: Int,
    // The amount the seller paid for the minUTxOValue, over what the bidder paid.
    seller_deposit: Int,
    // The size of the bid.
    bid: List<(Asset,Int)>,
    // The address where the payment output must go if the bidder claims it. The output must include
    // the seller's deposit + the bid amount.
    payment_address: Address,
    // The claim expiration, after which the seller can reclaim the NFTs plus the bidder's deposit.
    claim_expiration: PosixTime
  }
}
