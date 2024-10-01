# Revision history for Cardano-Aftermarket

## 2.0.0.0rc

#### New Features

Added another type of auction where the buyer can finalize the trade. This is required to enable
certain DApp compositions. For example, cardano-loans requires the buyer of a bond to update the
lender payment address in the same transaction as the purchase.

The two types of auctions are:
- Spot Auctions - auctions where the seller can immediately accept the bid and send the NFTs to the
buyer. The seller is the one to submit the final transaction.
- Claim Auctions - auctions where the seller will lock-up the NFTs with the chose bid UTxO and wait
for the bidder to come claim them. Claiming them requires paying the seller the bid amount specified
in the bid UTxO's datum. The bidder is the one to submit the final transaction. If the bidder does
not claim the NFTs in the agreed upon amount of time, the seller can reclaim the NFTs, and also
claim the bidder's deposit as compensation. Bidders can deliberately offer larger deposits to make
their bids more enticing for sellers.

## 1.0.0.0rc

#### Renamed

The protocol has been renamed to cardano-aftermarket. Since renaming github repos can open up
potential attack vectors (eg,
[repojacking](https://github.blog/2024-02-21-how-to-stay-safe-from-repo-jacking/)), the decision was
made to just archive the original cardano-secondary-market repo and create a brand new repo under
the new name.

#### New Features

*All* of the features discussed in the *Future Directions* section of the previous version's README
have been added:

- *Updating Multiple Sales* - you can now create/update/close/accept/purchase multiple sales in a
single transaction.
- *Batch Sales* - you can sell multiple NFTs in a single batch as long as they are all under the
same policy id.
- *Spot Markets & Auction Markets* - sellers can choose to sell their NFTs using an auction instead
of a spot sale. Potential buyers can see these auctions and make bids. The seller can then accept
any bid they like, as long as they send the required NFT to the bidder in the same transaction.
- *Script-based payment addresses* - sellers and bidders can now opt to use the pre-approved proxy
script for the address where direct payments must go. This proxy script just delegates authorization
to the address' staking credential which means users can now use arbitrary logic to protect their
payment addresses. For example, to use a multi-sig, the payment address would use the proxy script
as the payment credential and the multi-sig script as the staking credential. This feature is likely
very important for enabling adoption by institutions and corporations. 

Other new features that have been added:

- *Multi-Asset Sale Prices* - instead of asking for just a single asset in exchange of the NFTs,
sellers can ask for a basket of assets. To purchase the NFTs, all of the required assets and their
amounts must go to the seller's payment address. Bidders are also able to offer a basket of assets
in exchange for NFTs at auction.
- *Full Composability* - every action of this protocol is composable with all other actions. Here
are some examples:
    - You can buy NFTs from multiple sellers in the same transaction.
    - You can create a Bid for an auction and accept a bid for your own auction in the same
    transaction.
    - You can accept a bid for your action, convert another of your Spot UTxOs to an Auction UTxO,
    and purchase another seller's Spot UTxOs, all in one transaction.

#### Optimizations

The smart contracts have been re-written in aiken, and all of the "one per tx" restrictions have
been lifted. Every part of the protocol has seen huge performance improvements over the PlutusTx
version.

The logic for the protocol was broken over several smart contracts to allow fitting more features
into the protocol. The universal aftermarket spending script just delegates to one of the other
scripts which can either be executed as minting policies or staking scripts. Since all scripts can
be used as reference scripts, there was no drawback from splitting up the logic like this.

## 1.0.0 (MVP)

* First version.
* Released as [cardano-secondary-market](https://github.com/fallen-icarus/cardano-secondary-market).
