# Cardano-Aftermarket

A [p2p-DeFi](https://github.com/zhekson1/CSL-DeFi-Protocols) aftermarket protocol for buying/selling
NFTs on the Cardano Settlement Layer.

> **NOTE** This protocol was originally called
> [Cardano-Secondary-Market](https://github.com/fallen-icarus/cardano-secondary-market). To
> minimize the risk of
> [repojacking](https://github.blog/2024-02-21-how-to-stay-safe-from-repo-jacking/), a new repo was
> created instead of simply renaming the old one.

## Table of Contents
- [Abstract](#abstract)
- [Motivation](#motivation)
- [The Cardano-Aftermarket DeFi Protocol](#the-cardano-aftermarket-defi-protocol)
    - [Supported Features](#supported-features)
- [Specification](#specification)
    - [The Seller's Address](#the-sellers-address)
    - [Proxy Script](#proxy-script)
    - [UTxO Types](#utxo-types)
    - [Three Aiken Smart Contracts](#three-aiken-smart-contracts)
    - [Spot UTxO Actions](#spot-utxo-actions)
        - [Creating Spot UTxOs](#creating-spot-utxos)
        - [Closing Spot UTxOs](#closing-spot-utxos)
        - [Updating Spot UTxOs](#updating-spot-utxos)
        - [Purchasing Spot UTxOs](#purchasing-spot-utxos)
    - [Auction UTxO Actions](#auction-utxo-actions)
        - [Creating Auction UTxOs](#creating-auction-utxos)
        - [Closing Auction UTxOs](#closing-auction-utxos)
        - [Updating Auction UTxOs](#updating-auction-utxos)
    - [Bid UTxO Actions](#bid-utxo-actions)
        - [Creating Bid UTxOs](#creating-bid-utxos)
        - [Closing Bid UTxOs](#closing-bid-utxos)
        - [Updating Bid UTxOs](#updating-bid-utxos)
        - [Accepting Bid UTxOs](#accepting-bid-utxos)
- [Benchmarks and Fee Estimations (YMMV)](#benchmarks-and-fee-estimations-ymmv)
- [Features Discussion](#features-discussion)
- [Future Directions](#future-directions)
- [Conclusion](#conclusion)

## Abstract

Cardano-Aftermarket is a p2p-DeFi protocol for trading NFTs on the Cardano Settlement Layer (CSL).
Users deploy and interact with each others' script addresses, thereby creating permissionless and
high-availability NFT aftermarkets. This protocol is fully general and can support *all* possible
aftermarkets; each aftermarket is tagged with dedicated beacons to enable filtering for specific
markets. Both spot markets and auction markets are supported for all aftermarkets. Finally, as with
all p2p-DeFi protocols, users maintain custody, delegation control, and voting control of their
assets at all times.

## Motivation

Aftermarkets play a crucial role in enhancing the efficiency and dynamism of an economy,
particularly in the DeFi space. For example, imagine if Bob owns a bond, but needs to sell it before
maturity in order to raise some cash. Where would he go? What if he has no where to go? If Bob can't
be sure that he will be able to liquidate the bond in an emergency, he may view lending money in the
first place as being too risky. As a consequence, the primary lending/borrowing marketplace would be
smaller just because there is no viable aftermarket for the associated bonds. The same logic applies
to other financial markets like options and futures. 

Current NFT aftermarket solutions on Cardano have three main drawbacks. The first is that they tend to
rely on centralized entities for listing and finding assets. This introduces an availability
bottleneck; centralized entities cannot guarantee the same level of uptime and censorship resistance
as the underlying blockchain. In order for DeFi to be immune from influence by those in power, every
critical piece of the economy must be decentralized and censorship resistance. *Aftermarkets are
critical.*

The second drawback is that they lack composability with other DeFi protocols. This lack of
composability can dramatically increase financial risk for users. For a full explanation of this,
check out the Cardano-Loans
[README](https://github.com/fallen-icarus/cardano-loans/tree/v1.0.0.0rc?tab=readme-ov-file#no-trustless-composability-with-other-dapps).

The final drawback is that users must give up custody of their assets in order to use the
marketplaces. Contemporary DApps usually require users to pool their assets into a select set of
addresses. This has three critical issues:

1. Pooled assets are a much more enticing target for hackers and governments.
2. Users are forced to give up delegation control of all assets participating in DeFi. This has
   serious negative implications for the long-term security of Cardano.
3. Users are forced to give up voting control of all assets participating in DeFi. This puts the
   future or Cardano's decentralized government in jeopardy.

There are currently no true p2p aftermarkets. *Using the blockchain does not automatically make the
DApp p2p!* It also doesn't automatically make it decentralized. Cardano needs a radically different
kind of aftermarket DApp - one that is *actually* p2p, decentralized, and censorship resistant.

## The Cardano-Aftermarket DeFi Protocol

Cardano-Aftermarket is (arguably) the first truly p2p, decentralized, and censorship resistance DeFi
aftermarket. By using the distributed DApp design (ie, all users get their own personal DApp
address), it doesn't have any of the above limitations. Furthermore, this protocol is a general
purpose marketplace that can serve as the aftermarket for *all possible* financial markets. It can
even serve as an aftermarket for art NFTs.

### Supported Features

- **Spot Markets & Auction Markets** - sellers can choose to either put up their NFTs for sale at a
specified price *or* they can initiate an auction for the NFTs and accept bids from potential
buyers.
- **Batch Sales** - sellers can group NFTs from the same policy id together and sell/auction them as
a batch. For example, instead of selling 10 options contracts individually, Bob can offer them as a
package deal.
- **Multi-Asset Prices** - sellers and buyers can trade any number of assets in exchange for the
NFTs. For example, Alice can sell her options contract for 10 ADA + 10 DJED. Meanwhile, Bob can use
5 ADA, 10 AGIX, and 30 DJED to bid on a loan bond. *All Cardano native assets are supported.* It is
also possible to use only a single asset for the price, if desired.
- **Direct Payments** - when a seller accepts a bid for an Auction, they send the NFTs directly to
the winning bidder's specified payment address. Likewise, when a buyer purchases a spot NFT, they
send the proceeds directly to the seller's specified payment address. There is no need for the users
to return to the DApp address to claim their funds. There is even support for multi-sig and plutus
addresses as the payment addresses (using the proxy script).
- **Staking Script Support** - buyers and sellers can use staking scripts for their DApp addresses.
This enables using more complicated logic to protect their assets in the DApp.
- **A Single DApp Address for each Seller** - sellers only need to worry about a single address for
the entire protocol; it does not matter which NFTs are being sold. For example, Bob would use the
same DApp address for selling loan bonds *and* options contracts. This makes it very easy for
front-ends to integrate the protocol, and for users to manage their delegation and voting
preferences.
- **Democratic Upgradability** - users decide if and when to upgrade to a new version of the DApp.
No one can decide this for them, or force them to move to a new version (eg, by freezing all
functionality for the current version). This protocol is cross-version compatible; Bob can create a
new spot sale using v1 and create a bid on an auction that is using v2.
- **Full Composability** - this protocol is natively composable with all other DeFi DApps. It can
even be composed with itself: a seller can create a new auction in the same transaction where they
accept a bid for another auction.
- **Censorship Resistant** - any user that has the full UTxO set (ie, a full-node) automatically has
the entire market for *all* aftermarkets. No one can prevent them from seeing market activity,
and/or participating in the markets.

## Specification

This section is the low-level specification for the protocol. If you are only interested in the
high-level aspects, feel free to skip to the next [section](#benchmarks-and-fee-estimations-ymmv).

### The Seller's Address

Sellers each create a unique DApp address - this is where all of their sales/auctions take place. As
is common in *distributed dApps*, all such DApp addresses use the same validator script for the
payment credential, and a unique, user-defined staking key or script for the staking credential.
Owner-related actions are delegated to the staking credential *by* the validator script, so the user
maintains full control of all assets at the address. 

Since auctions occur in the seller's address, the seller maintains staking and voting rights over
all assets throughout the auction. This includes any bids made by potential buyers which
incentivizes bidders to be proactive and not leave bids open for too long.

Payments are *not* made to this address. Instead, the payment credential acts as an overseer to
ensure that the payment goes to the proper address.

> :warning: If, at any point, a misconfigured UTxO is sent to a seller's DApp address, the seller
> will get custody. Such UTxOs can only come about by misusing the protocol.

### Proxy Script

Since Cardano-Aftermarket has users make payments directly to addresses specified in the datums,
care must be taken when the specified address uses a payment plutus script. This is because the
protocol enforces a specific datum with payments in order to guarantee uniqueness of outputs - this
is currently the cheapest option for preventing double satisfaction. If the payments with this
enforced datum are sent to an address that requires a different datum, the payment could be locked
forever.

To address this issue, Cardano-Aftermarket uses the same Proxy Script as Cardano-Loans. The proxy
script can accept any datum, use any redeemer, and simply delegates spending authorization to the
address' staking credential, which can be anything (eg, a native script, a plutus script, or even a
pubkey).

Payment pubkey addresses are always allowed since they can accept any datum. The proxy script is
only necessary if the user wishes to use more than just a single pubkey to protect their proceeds.
For example, if the seller wishes to use a multisig, they would use the proxy script as the payment
credential and the native multisig script as the staking credential for their specified payment
address. If the seller wanted more complicated logic to protect their assets, they would use the
proxy script as the payment credential and a plutus script as the staking credential. *The proxy
script must always be paired with a staking credential.* The protocol will disallow any proxy
scripts used without a staking credential.

### UTxO Types

This protocol supports three types of UTxOs:

- Spot UTxOs - a UTxO dedicated to selling NFTs at a predefined price.
- Auction UTxOs - a UTxO dedicated to initiating an auction for the given NFTs.
- Bid UTxOs - a UTxO dedicated to bids for an associated Auction UTxO.

Spot UTxOs and Auction UTxOs are both created and managed by sellers while Bid UTxOs are created and
managed by buyers.

While Auction and Bid UTxOs are meant to be paired up, they do not need to be. For example, if a
buyer sees a Spot UTxO that hasn't sold at the desired price, they can create a Bid UTxO for those
NFTs at a different price; this is effectively a counter-offer even though it was not meant to be an
auction.

Each UTxO type has dedicated beacons:

#### Spot UTxO Beacons

- Spot beacon - a beacon with the token name "Spot" that represents the fact this UTxO is a Spot
UTxO.
- Policy beacon - a beacon representing the policy id for the NFTs being sold. It has the token
name: `sha2_256("00" ++ policy_id)`.

#### Auction UTxO Beacons

- Auction beacon - a beacon with the token name "Auction" that represents the fact this UTxO is an
Auction UTxO.
- Policy beacon - a beacon representing the policy id for the NFTs being auctioned. It has the token
name: `sha2_256("00" ++ policy_id)`.

#### Bid UTxO Beacons

- Bid beacon - a beacon with the token name "Bid" that represents the fact this UTxO is a Bid UTxO.
- Policy beacon - a beacon representing the policy id for the NFTs being auctioned. It has the token
name: `sha2_256("00" ++ policy_id)`.
- BidderId beacon - a beacon representing the bidder's staking credential. It has the token name:
`"01" ++ bidder_credential_hash`. This beacon enables bidders to find all of their bids located
across the sellers' DApp addresses.

### Three Aiken Smart Contracts

Due to the amount of logic required for this protocol and the desire to minimize the impact from
redundant executions, this protocol uses 3 separate smart contracts; *not all of them are required
with each transaction*. Each smart contract is dedicated to a specific purpose. Most user actions
only required 2 of the 3 contracts in a single transaction. Some actions do require all three.
However, since these scripts can be used as reference scripts, there is still plenty of room for
DApp composability.

- *Beacon Smart Contract* - this smart contract is in charge of minting/burning beacons for the
protocol. In addition to being a minting policy, it can also be executed as a staking script to
enable cheaply updating protocol UTxOs in-place (ie, no beacons need to be minted/burned, but the
new datums need to be checked).
- *Payment Observer Smart Contract* - this smart contract is in charge of observing all payments. It
can only be executed as a staking script.
- *Aftermarket Spending Smart Contract* - this smart contract is the payment credential for *all*
DApp addresses. It delegates checks to one of the other 2 smart contracts depending on the action
being taken. It can only be executed as a spending script.

The aftermarket spending smart contract hash is hard-coded into *all* of the other smart contracts to
enforce the use of the proper payment credential for all DApp addresses.

The payment observer smart contract hash is hard-coded into the beacon smart contract so that the
beacon smart contract can force the use of the proper observer logic. The protocol's complementary
proxy smart contract is also hard-coded into the beacon smart contract so that payment addresses can
be checked for proper configurations.

#### Aftermarket Spending Smart Contract Datums

Spot UTxOs have a `SpotDatum`.  
Auction UTxOs have an `AuctionDatum`.  
Bid UTxOs have an `BidDatum`.

```haskell
-- | A type synonym for an asset's full name.
type Asset = (CurrencySymbol,TokenName)

-- | The datum for a Spot UTxO sale for NFTs.
data SpotDatum = SpotDatum
  -- | The policy id for the beacon script.
  { beaconId :: CurrencySymbol
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being offered.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The address where the proceeds must go upon purchase of the Spot UTxO.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , saleDeposit :: Integer
  -- | The price for this batch, denominated in the specified assets. This is the price for the
  -- entire batch of NFTs. The payment output must contain *all* of the required assets, and their
  -- specified amounts.
  , salePrice :: [(Asset,Integer)]
  }

-- | The datum for an Auction UTxO for NFTs.
data AuctionDatum = AuctionDatum
  -- | The policy id for the beacon script.
  { beaconId :: CurrencySymbol
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being auctioned.
  , nftPolicyId :: CurrencySymbol
  -- | The token names of the NFTs offered in this batch.
  , nftNames :: [TokenName]
  -- | The desired starting price. This is only used to broadcast the auctioner's desired value.
  -- Bidders are able to create "counter-bids" with different assets and/or different nft names.
  -- For example, the bidder can make a bid for just one of the NFTs in the batch.
  , startingPrice :: [(Asset,Integer)]
  }

-- | The datum for a Bid UTxO for NFTs.
data BidDatum = BidDatum
  -- | The policy id for the beacon script.
  { beaconId :: CurrencySymbol
  -- | The hash of the payment observer script.
  , paymentObserverHash :: ScriptHash
  -- | The policy id for the NFTs being bid on.
  , nftPolicyId :: CurrencySymbol
  -- | The credential used for the BidderId token name. This is used so bidders can find all of 
  -- their bids despite them being located in other addresses. 
  , bidderCredential :: Credential
  -- | The token names of the NFTs being bid on.
  , nftNames :: [TokenName]
  -- | The address where the NFTs must go upon accepting the bid.
  , paymentAddress :: Address
  -- | The amount of ada used for the minUTxOValue.
  , bidDeposit :: Integer
  -- | The actual bid.
  , bid :: [(Asset,Integer)]
  }
```

#### Aftermarket Spending Smart Contract Redeemers
```haskell
data MarketRedeemer
  -- | Close or update either a Spot UTxO or an Auction UTxO.
  = CloseOrUpdateSellerUTxO
  -- | Close or update a Bid UTxO.
  | CloseOrUpdateBidderUTxO
  -- | Purchase a Spot UTxO.
  | PurchaseSpot
  -- | Accept a Bid UTxO, and close the associated Auction UTxO.
  | AcceptBid
```

#### Payment Observer Smart Contract Redeemers
```haskell
data PaymentObserverRedeemer
  -- | Observe a market payment transaction. This observes the payment for both spot purchases and
  -- bid acceptances.
  = ObservePayment { beaconId :: CurrencySymbol }
  -- | Register the script.
  | RegisterPaymentObserverScript
```

#### Beacon Smart Contract Redeemers
```haskell
data BeaconsRedeemer
  -- | Create, close, or update some market UTxOs (1 or more). 
  = CreateCloseOrUpdateMarketUTxOs
  -- | Burn any beacons. 
  | BurnBeacons
  -- | Register the script.
  | RegisterBeaconsScript
```

#### Payment Datum

This datum is attached to all direct outputs to payment addresses. It is used to prevent
double-satisfaction.

```haskell
-- | The `CurrencySymbol` is always the beacon policy id, and the `TxOutRef` is always
-- the output reference for either the Spot UTxO being purchased or the Bid UTxO being accepted.
newtype PaymentDatum = PaymentDatum (CurrencySymbol,TxOutRef)
```

The `CurrencySymbol` is included to prevent double-satisfaction during DApp composition; just using
the `TxOutRef` may not be enough to guarantee uniqueness.

### Spot UTxO Actions

#### Creating Spot UTxOs

At a high-level, creating Spot UTxOs involves creating the new UTxOs at the seller's DApp address
with the desired `SpotDatum`s, storing them with the NFTs in question, and tagging them with the
required beacons. The beacon smart contract will check all outputs containing beacons to ensure
invalid UTxOs are never broadcast to other users.

At a low-level, all of the following must be true:

- The beacon smart contract must be executed as a minting policy using
`CreateCloseOrUpdateMarketUTxOs`.
- All Spot UTxO outputs must be to a DApp address with a valid staking credential.
- All Spot UTxO outputs must have exactly two kinds of beacons, with exactly one unit of each:
    - a Spot beacon with the token name "Spot"
    - a Policy beacon with the token name corresponding to the `nftPolicyId` in the `SpotDatum`.
- All Spot UTxO outputs must have a valid inline `SpotDatum`:
    - `beaconId` == this policy id
    - `paymentObserverHash` == hard-coded payment observer hash
    - `nftPolicyId` == policy id for nfts being sold in the UTxO and cannot be the beacon policy id
    - `nftNames` == token names for nfts being sold in the UTxO and cannot be empty
    - `paymentAddress` must either use a payment pubkey, or the proxy script as the payment
    credential and a valid staking credential
    - `saleDeposit` > 0
    - `salePrice` must not be empty, must not use any protocol beacons as the assets, all prices
    must be > 0, and the list must be sorted.
- All Spot UTxO outputs must have the `saleDeposit` amount of ada + the NFTs being sold.

The `nftNames` is the list of all NFTs being sold in this batch.

The `paymentAddress` is where the proceeds will go when the NFTs are purchased.

The `salePrice` is the price of the NFT batch. It can be a basket of assets. This amount will be
sent to the `paymentAddress` when the batch is purchased.

The `saleDeposit` is the amount of ada used for the minUTxOValue. This will be sent to the
`paymentAddress` in addition to the required `salePrice` assets.

#### Closing Spot UTxOs

At a high-level, closing Spot UTxOs involves spending the target UTxOs at the seller's DApp address,
and burning the beacons attached to them. The seller must approve this transaction. The beacon smart
contract will check all beacons are properly burned to ensure invalid UTxOs are never broadcast to
other users.

At a low-level, all of the following must be true:

- The aftermarket spending smart contract is executed for the Spot UTxO input using
`CloseOrUpdateSellerUTxO`.
- The Spot UTxO must have a `SpotDatum`.
- The DApp address' staking credential must signal approval.
- If the Spot UTxO being spent contains beacons:
    - The beacon smart contract must be executed as a minting policy using
    `CreateCloseOrUpdateMarketUTxOs`.

The beacon smart contract will actually do the exact same check as when creating Spot UTxOs.
However, since closing Spot UTxOs implies no new Spot UTxO outputs, there are no outputs to check.

If there is ever an invalid Spot UTxO (ie, a UTxO with a `SpotDatum` but no beacons), it can be
spent with this method; the beacon smart contract would not need to be executed. 

#### Updating Spot UTxOs

Updating Spot UTxOs in-place can be done regardless of whether beacons must be changed. The
steps are identical to closing Spot UTxOs, except you now create Spot UTxO outputs as well.
Since there are now outputs, the outputs will be checked by the beacon script and must
comply with the same requirements as when creating Spot UTxOs.

If no beacons need to be minted/burned, the beacon script must be executed as a staking script using
`CreateCloseOrUpdateMarketUTxOs`. If beacons *do* need to be minted/burned, then the beacon script
must be executed as a minting policy using the same redeemer.

#### Purchasing Spot UTxOs

At a high-level, purchasing a spot batch involves sending the payment to the seller's payment
address, and burning all protocol beacons.

At a low-level, all of the following must be true:

- The payment observer script must be executed as a staking script using `ObservePayment`.
- The following must be true for all Spot UTxO inputs being purchased:
    - The input must be spent using `PurchaseSpot`.
    - The input must have a `SpotDatum`.
    - The input must have the required beacons.
    - There must be a corresponding spot payment output with the following characteristics:
        - It must be locked at the `paymentAddress` in the `SpotDatum`.
        - It must contain the `salePrice` asset values + the `saleDeposit` amount of ada.
        - It must contain the inline `PaymentDatum` with the beacon policy id as the
        `CurrencySymbol` and the input's output reference as the `TxOutRef`.
    - Either all beacons attached to the input must be burned or the beacon script must be executed
      using `CreateCloseOrUpdateMarketUTxOs`.

If all beacons need to be burned, it is cheaper to use `BurnBeacons`. However, if beacons are being
recycled into new outputs or new beacons need to be minted, then `CreateCloseOrUpdateMarketUTxOs`
should be used. If all beacons are being recycled *and* no minting is required, the beacon script
must be executed as a staking script.

*Spot payment outputs must be in the same order as the Spot inputs!* They do *not* need
to be paired up. You can even have unrelated outputs between the required outputs. This ordering
restriction helps with performance.

### Auction UTxO Actions

#### Creating Auction UTxOs

At a high-level, creating Auction UTxOs involves creating the new UTxOs at the seller's DApp address
with the desired `AuctionDatum`s, storing them with the NFTs in question, and tagging them with the
required beacons. The beacon smart contract will check all outputs containing beacons to ensure
invalid UTxOs are never broadcast to other users.

At a low-level, all of the following must be true:

- The beacon smart contract must be executed as a minting policy using
`CreateCloseOrUpdateMarketUTxOs`.
- All Auction UTxO outputs must be to a DApp address with a valid staking credential.
- All Auction UTxO outputs must have exactly two kinds of beacons, with exactly one unit
of each:
    - an Auction beacon with the token name "Auction"
    - a Policy beacon with the token name corresponding to the `nftPolicyId` in the `AuctionDatum`.
- All Auction UTxO outputs must have a valid inline `AuctionDatum`:
    - `beaconId` == this policy id
    - `paymentObserverHash` == hard-coded payment observer hash
    - `nftPolicyId` == policy id for nfts being auctioned in the UTxO and cannot be the beacon
    policy id
    - `nftNames` == token names for nfts being auctioned in the UTxO and cannot be empty
    - `startingPrice` must not be empty, must not use any protocol beacons as the assets,
    all prices must be > 0, and the list must be sorted.
- All Auction UTxO outputs must have the NFTs being auctioned.

The `nftNames` is the list of all NFTs being auctioned in this batch.

The `startingPrice` is just meant to broadcast the seller's desired starting price for the auction.
The seller is free to accept bids lower than this price, if desired. They can even accept bids for
only a subset of the NFTs in the batch.

#### Closing Auction UTxOs

At a high-level, closing Auction UTxOs involves spending the target UTxOs at the seller's DApp address,
and burning the beacons attached to them. The seller must approve this transaction. The beacon smart
contract will check all beacons are properly burned to ensure invalid UTxOs are never broadcast to
other users.

At a low-level, all of the following must be true:

- The aftermarket spending smart contract is executed for the Auction UTxO input using
`CloseOrUpdateSellerUTxO`.
- The Auction UTxO must have an `AuctionDatum`.
- The DApp address' staking credential must signal approval.
- If the Auction UTxO being spent contains beacons:
    - The beacon smart contract must be executed as a minting policy using
    `CreateCloseOrUpdateMarketUTxOs`.

The beacon smart contract will actually do the exact same check as when creating Auction UTxOs.
However, since closing Auction UTxOs implies no new Auction UTxO outputs, there are no outputs to
check.

If there is ever an invalid Auction UTxO (ie, an UTxO with an `AuctionDatum` but no beacons), it can be
spent with this method; the beacon smart contract would not need to be executed. 

#### Updating Auction UTxOs

Updating Auction UTxOs in-place can be done regardless of whether beacons must be changed. The steps
are identical to closing Auction UTxOs, except you now create Auction UTxO outputs as well. Since
there are now outputs, the outputs will be checked by the beacon script and must comply with the same
requirements as when creating Auction UTxOs.

If no beacons need to be minted/burned, the beacon script must be executed as a staking script using
`CreateCloseOrUpdateMarketUTxOs`. If beacons *do* need to be minted/burned, then the beacon script
must be executed as a minting policy using the same redeemer.

### Bid UTxO Actions

#### Creating Bid UTxOs

At a high-level, creating Bid UTxOs involves creating the new UTxOs at the target seller's DApp
address with the desired `BidDatum`s, storing them with the bid amount offered, and tagging them
with the required beacons. The beacon smart contract will check all outputs containing beacons to
ensure invalid UTxOs are never broadcast to other users.

At a low-level, all of the following must be true:

- The beacon smart contract must be executed as a minting policy using
`CreateCloseOrUpdateMarketUTxOs`.
- All Bid UTxO outputs must be to a DApp address with a valid staking credential.
- All Bid UTxO outputs must have exactly two kinds of beacons, with exactly one unit of each:
    - a Bid beacon with the token name "Bid"
    - a BidderId beacon with the token name corresponding to the `bidderCredential` in the
    `BidDatum`.
    - a Policy beacon with the token name corresponding to the `nftPolicyId` in the `BidDatum`.
- All Bid UTxO outputs must have a valid inline `BidDatum`:
    - `beaconId` == this policy id
    - `paymentObserverHash` == hard-coded payment observer hash
    - `bidderCredential` == credential for the bidder.
    - `nftPolicyId` == policy id for nfts being sold in the UTxO and cannot be the beacon policy id
    - `nftNames` == token names for nfts being sold in the UTxO and cannot be empty
    - `paymentAddress` must either use a payment pubkey, or the proxy script as the payment
    credential and a valid staking credential
    - `bidDeposit` > 0
    - `bid` must not be empty, must not use any protocol beacons as the assets, all prices
    must be > 0, and the list must be sorted.
- All Bid UTxO outputs must have the `bidDeposit` amount of ada and `bid` asset value.
- All `bidderCredential`s used in the new `BidDatum` must approve the transaction.

The `nftNames` is the list of all NFTs being ask for in exchange for the bid assets.

The `paymentAddress` is where the NFTs will go when the bid is accepted.

The `bid` is the actual value of the bid for the target NFTs. It can be a basket of assets.

The `bidDeposit` is the amount of ada used for the minUTxOValue. This will be sent to the
`paymentAddress` in addition to the required NFTs.

The `bidderCredential`s must approve the transaction so that only the user who controls that
credential can use the associated BidderId beacon.

The bid can have different terms than the associated Auction UTxO. For example, the bid could ask
for different NFTs and/or offer different assets than the `startingPrice` from the `AuctionDatum`.

#### Closing Bid UTxOs

At a high-level, closing Bid UTxOs involves spending the target UTxOs at the target seller's DApp
address, and burning the beacons attached to them. The bidder must approve this transaction. The
beacon smart contract will check all beacons are properly burned to ensure invalid UTxOs are never
broadcast to other users.

At a low-level, all of the following must be true:

- The aftermarket spending smart contract is executed for the Bid UTxO input using
`CloseOrUpdateBidderUTxO`.
- The Bid UTxO must have a `BidDatum`.
- If the Bid UTxO being spent contains beacons:
    - The `bidderCredential` must signal approval.
    - The beacon smart contract must be executed as a minting policy using
    `CreateCloseOrUpdateMarketUTxOs`.
- If no beacons are present, the address' staking credential must approve.

The beacon smart contract will actually do the exact same check as when creating Bid UTxOs.
However, since closing Bid UTxOs implies no new Bid UTxO outputs, there are no outputs to
check.

If there is ever an invalid Bid UTxO (ie, an UTxO with an `BidDatum` but no beacons), it can be
spent by the *address owner* with this method; the beacon smart contract would not need to be
executed. 

#### Updating Bid UTxOs

Updating Bid UTxOs in-place can be done regardless of whether beacons must be changed. The steps are
identical to closing Bid UTxOs, except you now create Bid UTxO outputs as well. Since there are now
outputs, the outputs will be checked by the beacon script and must comply with the same requirements
as when creating Auction UTxOs.

If no beacons need to be minted/burned, the beacon script must be executed as a staking script using
`CreateCloseOrUpdateMarketUTxOs`. If beacons *do* need to be minted/burned, then the beacon script
must be executed as a minting policy using the same redeemer.

#### Accepting Bid UTxOs

At a high-level, accepting a bid involves sending the NFTs to the bidder's payment address, and
burning all protocol beacons.

At a low-level, all of the following must be true:

- The payment observer script must be executed as a staking script using `ObservePayment`.
- The following must be true for all Bid UTxO inputs being accepted:
    - The input must be spent using `AcceptBid`.
    - The input must have a `BidDatum`.
    - The input must have the required beacons.
    - There must be a corresponding bid payment output with the following characteristics:
        - It must be locked at the `paymentAddress` in the `BidDatum`.
        - It must contain the NFTs + the `bidDeposit` amount of ada.
        - It must contain the inline `PaymentDatum` with the beacon policy id as the
        `CurrencySymbol` and the input's output reference as the `TxOutRef`.
    - Either all beacons attached to the input must be burned or the beacon script must be executed
      using `CreateCloseOrUpdateMarketUTxOs`.

If all beacons need to be burned, it is cheaper to use `BurnBeacons`. However, if beacons are being
recycled into new outputs or new beacons need to be minted, then `CreateCloseOrUpdateMarketUTxOs`
should be used. If all beacons are being recycled *and* no minting is required, the beacon script
must be executed as a staking script.

*Bid payment outputs must be in the same order as the Bid inputs!* They do *not* need to be paired
up. You can even have unrelated outputs between the required outputs. This ordering restriction
helps with performance.

It is possible to accept bids that have different terms than the associated Auction UTxO. For
example, you can accept a bid asking for only some of the NFTs being auctioned. You can then
rollover the leftover NFTs into a new Auction UTxO in the same transaction.

If the NFTs required for the bid payment are currently in an Auction UTxO, that UTxO can be closed
in the same transaction using `AcceptBid` as the spending redeemer. If the NFTs are in a Spot UTxO,
that UTxO can be closed using `CloseOrUpdateSellerUTxO` as the spending redeemer, however, the
beacons script must then be executed using `CreateCloseOrUpdateMarketUTxOs` with either a minting
execution or staking execution (depending on whether beacons must be minted/burned).

## Benchmarks and Fee Estimations (YMMV)

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found [here](./Benchmarks.md). The following table provides a quick
summary. Only the worst case benchmarks are shown. The `Max Tx Fee` is the transaction fee for the
worst case scenario while the `Min Tx Fee` is the fee if only one action was taken in that scenario
(eg, only 1 auction was created, or 1 spot batch was purchased). 

| Action | Worst Case | Max Tx Fee | Min Tx Fee |
|:--:|:--:|:--:|:--:|
| Creating Spot Batches | 32 spots/tx | 1.722147 ADA | 0.236636 ADA |
| Updating Spot Batches | 21 spots/tx | 1.930661 ADA | 0.253392 ADA |
| Closing Spot Batches | 31 spots/tx | 1.625099 ADA | 0.204058 ADA |
| Purchasing Spot Batches | 22 spots/tx | 1.682365 ADA | 0.258371 ADA |
| Creating Auction Batches | 35 auctions/tx | 1.528574 ADA | 0.227490 ADA |
| Updating Auction Batches | 22 auctions/tx | 2.129716 ADA | 0.393530 ADA |
| Closing Auction Batches | 32 auctions/tx | 1.661041 ADA | 0.202913 ADA |
| Creating Bids | 21 bids/tx | 1.970657 ADA | 0.280369 ADA |
| Updating Bids | 14 bids/tx | 1.764683 ADA | 0.296901 ADA |
| Closing Bids | 31 bids/tx | 1.663468 ADA | 0.206458 ADA |
| Accepting Bids | 10 bids/tx | 1.485366 ADA | 0.293522 ADA |

## Features Discussion

### Native Support For *All* DeFi Financial Assets

This marketplace is fully general and can work with all possible DeFi markets that offer tradable
NFTs. Since all primary markets will likely have predefined minting policies for their financial
assets, the associated aftermarkets can easily be found using only those minting policies. This is
entirely thanks to the beacon tokens; they are the reason all possible aftermarkets are supported
despite using only a single DApp address per user. The beacon tokens are also why this protocol
supports financial markets that have not even been invented yet; no permission or governance action
are required to create new aftermarkets.

### Native Support For *All* Native Assets

Buyers and sellers can exchange *any* assets for the NFTs. This includes assets that do not even
exist yet. Even other NFTs can be offered! No permission or governance action is needed to add
support for new assets.

### Trustless Negotiations

While only auctions are meant to use bids, the bids can actually be used to create counter-offers of
both spot sales and auctions. Imagine if Alice is trying to sell two bonds using a spot sale for 100
ADA (50 ADA each). What if Bob only wants one of the bonds? He could use a bid to tell Alice he'll
take only the first bond and pay 70 ADA for it. In other words, Bob is willing to pay up a little
just to get the single bond; he would still save money since he doesn't even want the second bond.
Alice can see this and choose to accept Bob's bid even though she didn't initiate an auction. She
can close the Spot UTxO, accept Bob's Bid UTxO, and create a new Spot UTxO with the remaining bond,
all in one transaction.

Because of the fully composable nature of spots, auctions, and bids, very complex market
interactions can trustlessly occur. All without any middlemen/batchers.

### Support For Art NFTs

Art assets have aftermarkets just like financial assets, and this protocol can also be used to trade
art NFTs in a fully p2p fashion. However, this is not the intended purpose for this protocol. There
is no support for royalties, and there likely never will be since financial assets do not include
royalties.

## Future Directions

### Advanced Filtering of Aftermarkets

Currently, the protocol does not support using beacons to further filter aftermarkets. For example,
while it is possible to filter for all spot sales for option contracts, it is not currently possible
to further filter for options contracts where ada is the offered asset. The only way to securely
enable support for this is if the associated options contract was referenced when the corresponding
aftermarket sale/auction was created. This would prevent creating the wrong beacons for each sale.

While this is technically possible to do, it would add some substantial complexity to the protocol
and would dramatically impact overall performance when creating sales. Given the already high
throughput when creating sales, this may be acceptable but it may not be necessary.

Off-chain infrastructure could potentially fill this niche. For example, when a new aftermarket sale
is created, the off-chain infrastructure could automatically look up the associated financial
asset's information and store them together in its database. Then, this off-chain infrastructure can
offer filtering capabilities when users query those sales; it can even return the associated
information for the financial asset in the same query. A decentralized Cardano database like Koios
could fill this niche without sacrificing decentralization.

If the off-chain infrastructure is unable to satisfy this niche without sacrificing
decentralization, then support for this filtering can be added at the protocol level.

## Conclusion

Cardano-Aftermarket is a vital addition to the [p2p-DeFi protocol
family](https://github.com/zhekson1/CSL-DeFi-Protocols). It enables the formation of a radically
permissionless and highly composable NFT marketplace on the CSL, and works synergistically with
other p2p-DeFi protocols. 
