use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{PolicyId, AssetName}

use cardano_aftermarket/data/core.{Asset, ScriptHash}

pub type MarketDatum {
  // A datum for a sale ready for immediate fulfillment by a buyer. It will be stored with a
  // "Spot" beacon. It must be stored with the actual NFTs being sold.
  SpotDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The payment observer script hash.
    payment_observer_hash: ScriptHash,
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

  // A datum for an auction looking for bids. It will be stored with an "Auction" beacon.
  // It must be stored with the actual NFTs being auctioned to prove the auctioner actually
  // controls them.
  AuctionDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The payment observer script hash.
    payment_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The token names for the NFTs being auctioned. The names must be sorted.
    nft_names: List<AssetName>,
    // The desired starting price. This is only used to broadcast the auctioner's desired value.
    // Bidders are able to create "counter-bids" with different assets and/or different nft names.
    // For example, the bidder can make a bid for just one of the NFTs in the batch.
    starting_price: List<(Asset,Int)>
  }

  // A datum for a bid for an auction. It will be stored with a "Bid" beacon. It must be stored
  // with the bid amount.
  BidDatum {
    // The policy id for the beacon script.
    beacon_policy_id: PolicyId,
    // The payment observer script hash.
    payment_observer_hash: ScriptHash,
    // The policy id for the NFTs being auctioned.
    nft_policy_id: PolicyId,
    // The credential used for the BidderId token name. This is used so bidders can find all of 
    // their bids despite them being located in other addresses. It is the hash of their staking 
    // credential.
    bidder_credential: Credential,
    // The token names of the NFTs the bid is for. This does not have to be the same as the list in
    // the corresponding Auction UTxO. The names must be sorted.
    nft_names: List<AssetName>,
    // The address where the payment output must go if the bid is accepted.
    payment_address: Address,
    // The amount the bidder paid for the minUTxOValue. This will be returned in the payment output.
    bid_deposit: Int,
    // The size of the bid.
    bid: List<(Asset,Int)>
  }
}
