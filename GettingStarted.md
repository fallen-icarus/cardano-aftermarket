# Getting Started

The `cardano-aftermarket` CLI assumes that all transactions are built and signed using
`cardano-cli`. **Access to a local node is not necessary**, although it does simplify things. Koios
can be used for all steps that require access to a node.

Template bash scripts that follow these steps are available [here](scripts/). There are only
examples using a local node. Using a remote node requires extra steps since the transaction must be
manually balanced. If you would like to use a remote node, the `cardano-aftermarket` CLI supports
everything you need. You can cross-reference these local node template scripts with the
[cardano-swaps](https://github.com/fallen-icarus/cardano-swaps/tree/main/scripts) remote node
template scripts to come up with your own remote node template scripts for cardano-aftermarket.

## Table of Contents
- [Installing](#installing)
- [Aiken For Developers](#aiken-for-developers)
- [Overspent Budget](#overspent-budget)
- [Using Remote Nodes](#using-remote-nodes)
- [Minting Test Tokens](#minting-test-tokens)
- [Registering Scripts - DEVELOPERS ONLY](#registering-the-scripts-for-staking-execution---developers-only)
- [Creating Reference Scripts](#creating-reference-scripts)
- [Creating a Spot UTxO](#creating-a-spot-utxo)
- [Closing a Spot UTxO](#closing-a-spot-utxo)
- [Updating a Spot UTxO](#updating-a-spot-utxo)
- [Purchasing a Spot UTxO](#purchasing-a-spot-utxo)
- [Creating an Auction UTxO](#creating-an-auction-utxo)
- [Closing an Auction UTxO](#closing-an-auction-utxo)
- [Updating an Auction UTxO](#updating-an-auction-utxo)
- [Creating a SpotBid UTxO](#creating-a-spotbid-utxo)
- [Closing a SpotBid UTxO](#closing-a-spotbid-utxo)
- [Updating a SpotBid UTxO](#updating-a-spotbid-utxo)
- [Accepting a SpotBid UTxO](#accepting-a-spotbid-utxo)
- [Creating a ClaimBid UTxO](#creating-a-claimbid-utxo)
- [Closing a ClaimBid UTxO](#closing-a-claimbid-utxo)
- [Updating a ClaimBid UTxO](#updating-a-claimbid-utxo)
- [Accepting a ClaimBid UTxO](#accepting-a-claimbid-utxo)
- [Claiming an AcceptedBid UTxO](#claiming-an-accepted-bid-utxo)
- [Unlocking an Unclaimed AcceptedBid UTxO](#unlocking-an-unclaimed-accepted-bid-utxo)
- [Time Conversions](#time-conversions)
- [Queries](#queries)
  - [Querying Personal Addresses](#querying-personal-addresses)
  - [Querying Spot UTxOs](#querying-spot-utxos)
  - [Querying Auction UTxOs](#querying-auction-utxos)
  - [Querying Bid UTxOs](#querying-bid-utxos)

## Installing

Make sure `cardano-cli` is also installed. You can get the most up-to-date copy from IOG's
cardano-node repo [here](https://github.com/IntersectMBO/cardano-node/releases). It will be in the
cardano-node tarball under the latest release.

### Install the necessary packages - similar to cardano-node
```bash
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

### Install GHC and Cabal
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- Prepend or append the required PATH variable.
- You do not need to install the haskell-langauge-server.
- You do not need to install stack.
- Install the required packages. You can keep this terminal window open and install from another
window.
- Press ENTER to proceed.

```bash
source $HOME/.bashrc
ghcup install ghc 9.6.5
```

### Install libsodium, scep256k1, and blst
```bash
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../ # Leave the libsodium directory.
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig

cd ../ # Leave the secp256k1 directory.
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF # This command extends until the next EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

You need to execute the following to make the new packages usable:
```bash
echo '' >> $HOME/.bashrc # Add a newline to your .bashrc file.
echo 'export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"' >> $HOME/.bashrc
echo 'export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"' >> $HOME/.bashrc
source $HOME/.bashrc
```

### Build the executable - this may take about 30 minutes
```bash
cd ../ # Leave the blst directory.
git clone https://github.com/fallen-icarus/cardano-aftermarket
cd cardano-aftermarket
cabal clean
cabal update
cabal build exe:cardano-aftermarket
```

The `cardano-aftermarket` CLI program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.6.5/cardano-aftermarket-1.0.0.0/x/cardano-aftermarket/build/cardano-aftermarket/cardano-aftermarket`.
Move the program to somewhere in your `$PATH`.

All `cardano-aftermarket` subcommands have an associated `--help` option. The functionality is meant to
feel like `cardano-cli`.

The smart contracts are compiled *into* the created `cardano-aftermarket` CLI. The executable has
everything you need for using the protocol. It is a batteries included CLI.

## Aiken For Developers

The aiken smart contracts come precompiled but if you would like to make changes or wish to confirm
the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using
cargo like this:

```bash
cargo install aiken --version 1.0.26-alpha
```

Make sure you install verison 1.0.26-alpha. Newer versions may change some things and so the source
code may not compile or may result in a different script. As aiken stabilizes, the code will be
updated to the latest version.

> [!TIP] 
> If the above command doesn't work, you can build aiken from source:
> ```bash
> git clone https://github.com/aiken-lang/aiken
> cd aiken
> git checkout v1.0.26-alpha
> cargo build
> ```
> The executable should now be located at `target/debug/aiken`.

When building the protocol's blueprints, make sure to use

```bash
aiken build -f user-defined -t verbose
```

or else the user friendly error messages will be stripped from the smart contracts and the resulting
beacons will be different.

If you would like to use the `cardano-aftermarket` CLI after making your changes, you will need to
rebuild it with `cabal bulid exe:cardano-aftermarket`. As long as you did not make any breaking changes,
the CLI should still work for you.

If you would like to test your changes, you can run the tests using `cabal run tests`. As long
as you did not make any breaking changes, the tests should quickly give you feedback. There are
four kinds of tests:

1) Regression tests - tests for features that should work.
2) Failure tests - tests for scenarios that are supposed to fail.
3) Bench tests - tests to check for degraded performance in specific scenarios.
4) Performance Increase tests - tests to check for improved performance in specific scenarios; these
tests will fail if performance increases to alert you of the change.

To see the documentation for the tests, you can build the haddocks for the tests using `cabal
haddock tests`. The documentation may be easier to read than the source code. You can view the
documentation in any browser.

## Overspent Budget

While `cardano-cli` is able to auto-balance transactions, the auto-balancer does not always work
when scripts are executed in a transaction where native tokens must go to the change address. It
does not properly add the change *before* estimating the execution budgets for the transaction which
always results in it under-estimating the required execution units needed by the scripts. There are
open issues about this [here](https://github.com/input-output-hk/cardano-node/issues/5386) and
[here](https://github.com/input-output-hk/cardano-api/issues/302). If you ever see a very long and
confusing error message while using `cardano-cli transaction build`, this is probably the issue.

As a work around, whenever you build a transaction using `cardano-cli transaction build` where
scripts are being executed, you must manually create an output that has all of the native tokens
that would normally go into the change output. You can let the auto-balancer balance the ada.

## Using Remote Nodes

`cardano-cli transaction build` requires a local node for the auto-balancer which means it cannot be
used to build a transaction for a remote node. Instead, the `cardano-cli transaction build-raw` 
command is required. This command requires the following steps:
1. Build a temporary transaction that is missing the execution units and transaciton fee but is
   properly balanced. You can assume a fee of zero for this transaction.
2. Submit the temporary transaction for execution budget estimations.
3. Rebuild the transaction with the proper execution budgets. The fee is still set to zero.
4. Calculate the required fee for this new temporary transaction.
5. Create the final transaction with the required fee and properly balanced outputs (subtract off
   the fee from the change).
6. Sign the transaction and submit to a remote node.

The `cardano-aftermarket` CLI uses [Koios](https://koios.rest/) in all scenarios where a node is required.

##### Querying protocol parameters

Some of the above steps will require the current protocol parameters. The `cardano-aftermarket` CLI
can be used to fetch the current protocol parameters using Koios. The parameters are already
formatted in the way `cardano-cli` requires. To fetch the parameters, you can use:

```bash
cardano-aftermarket query protocol-params \
  --testnet \
  --out-file protocolParams.json
```

##### Estimating execution budgets

Submitting a transaction for execution budget estimations can be done with this command:

```bash
cardano-aftermarket evaluate-tx \
  --testnet \
  --tx-file tx.body
```

This action uses Koios. The returned budgets will be indexed by the input order, policy id order,
and withdrawal credential order. **This may not be the same order you specified when building the
temporary transaction.** The node will reorder them based on lexicographical ordering. If you are
not sure of the proper ordering, you can view the transaction file that is created with
`cardano-cli` using `cardano-cli transaction view`; the inputs, policy ids, and withdrawal
credentials will be properly ordered.

##### Submitting the final transaction

Submitting the final transaction for addition to the blockchain can be done with this command:

```bash
cardano-aftermarket submit \
  --testnet \
  --tx-file tx.signed
```

The transaction will be submitted through Koios.

## Minting Test Tokens

An always succeeding minting policy as well as the required redeemer are included with template bash
scripts for a local node. These can be used to create as many native tokens as needed to test this
protocol.

To see how to mint test tokens using a local node, refer
[here](scripts/local-node/mint-test-tokens/)

## Registering the scripts for staking execution - DEVELOPERS ONLY

**This action only needs to be done once per network for the entire protocol. It does not need to be
done by any users.** These instructions are for completeness as they may be needed by developers.

The plutus scripts cannot be executed as staking scripts until after they are registered. Once the
scripts are registered, they can be used as staking scripts immediately by all users. Registering
the scripts does not require executing the scripts (this may change in the future). Once they are
registered, *they cannot be deregistered*.

**Registration has already been done for all scripts for the preproduction testnet.**

The following protocol scripts requires staking executions, and therefore, require registration:
- Beacon Script
- Payment Observer Script

Registering the scripts involve:
1. Exporting each script from the `cardano-aftermarket` CLI.
2. Exporting the required redeemers for each script.
3. Creating a registration certificate for each script.
4. Submitting a transaction that also pays the registration deposit (2 ADA per registration).

All scripts can be registered in a single transaction.

##### Exporting the scripts
```bash
cardano-aftermarket scripts \
  --beacon-script \
  --out-file beacons.plutus

cardano-aftermarket scripts \
  --payment-script \
  --out-file payment_observer.plutus
```

##### Exporting the redeemers
```bash
cardano-aftermarket redeemers beacon-script register \
  --out-file beacon_redeemer.json

cardano-aftermarket redeemers observer-script register \
  --out-file observer_redeemer.json
```

##### Create the registration certificates
```bash
cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file beacons.plutus \
  --out-file beacons.cert

cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file payment_observer.plutus \
  --out-file payment_observer.cert
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local-node/create-reference-scripts/register-scripts.sh)

## Creating Reference Scripts

**Do not skip this step!** While beacon tokens can be used to trustlessly share reference scripts,
this has not been set up for the beta testing. For now, you will need your own reference scripts.

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-aftermarket` CLI.
2. Submit a transaction with the reference script stored in the outputs.

Due to the sizes of the scripts, each script will require its own transaction.

You can see examples [here](scripts/local-node/create-reference-scripts/).

## Creating a Spot UTxO

Creating a Spot UTxO involves the following steps:
1. Create your seller address.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required SpotDatum for each Spot UTxO you wish to create.
5. Submit a transaction that creates the Spot UTxOs.

#### Creating your seller address
```bash
# Export the spending script.
cardano-aftermarket scripts aftermarket-script \
  --out-file aftermarket.plutus

# Create the seller address.
cardano-cli address build \
  --payment-script-file aftermarket.plutus \
  --stake-verification-key-file seller_stake.vkey \
  --testnet-magic 1 \
  --out-file seller.addr
```

Cardano-Aftermarket also supports using staking scripts for the address. To use a staking script,
use the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

#### Calculate the required beacon names to mint

Spot UTxOs require two beacons: 
- Spot beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
spotBeaconName=$(cardano-aftermarket beacon-name spot-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --stdout)

spotBeacon="${beaconPolicyId}.${spotBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the Spot Datum
```bash
cardano-aftermarket datums spot \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --nft-name '4f74686572546f6b656e0a' \
  --nft-name '54657374546f6b656e31' \
  --payment-address $(cat sellerPersonal.addr) \
  --seller-deposit 5000000 \
  --price '10000000 lovelace' \
  --price '10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000' \
  --out-file 'spotDatum.json'
```

The `nft-policy-id` flag must match the policy id used for the Policy beacon.

You can specify as many `nft-name`s as you want; these are the NFTs (under the specified policy id)
that are for sale in this batch. *These fields must be adjacent to each other.*

The `payment-address` is where the payment must go when the batch is purchased by someone.

The `seller-deposit` is the amount of ada used for the minUTxOValue. This will be returned to you when the
batch is purchased.

The `price` fields make up the sale price for this batch. Purchasing the batch requires *all* of the
specified assets. The above example has the price of:  
`10000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000`

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Building the transaction

Each Spot UTxO must be stored with the NFTs being sold in the batch.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-spot.sh).

## Closing a Spot UTxO

Closing a Spot UTxO involves the following steps:
1. Calculate the hash of the seller's staking credential.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required spending script redeemer.
5. Submit a transaction that closes the Spot UTxOs.

#### Calculate the hash of the staking credential used in the seller address
```bash
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file seller_stake.vkey)
```

#### Calculate the required beacon names to burn

Spot UTxOs have two beacons: 
- Spot beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
spotBeaconName=$(cardano-aftermarket beacon-name spot-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --stdout)

spotBeacon="${beaconPolicyId}.${spotBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Create the required spending redeemer
```bash
cardano-aftermarket redeemers market-script manage-seller-utxo \
  --out-file 'closeOrUpdateSellerUTxO.json'
```

##### Building the transaction
The seller's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-spot.sh).

## Updating a Spot UTxO

The steps to update Spot UTxOs are exactly the same as closing them, except you will need to
create the new outputs. All redeemers are the same.

By cross-referencing the [creation script](scripts/local-node/create-spot.sh) and the [closing
script](scripts/local-node/close-spot.sh), you can easily come up with your own update script.

It is also possible to convert a Spot UTxO to an Auction UTxO.

## Purchasing a Spot UTxO

Purchasing a Spot UTxO requires:

1. Create the required spending script redeemer.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required datum for the payments outputs.
5. Create the required staking address for the observer script.
6. Create the required observer script redeemer.
7. Submit the transaction.

#### Create the observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-aftermarket scripts \
  --observer-script \
  --out-file observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file observer.plutus)
```

#### Create the required redeemers
```bash
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file 'burnAllBeacons.json'

cardano-aftermarket redeemers market-script purchase-spot \
  --out-file 'purchaseSpot.json'

cardano-aftermarket redeemers observer-script observe \
  --out-file 'observe.json'
```

#### Create the payment datum
```bash
cardano-aftermarket datums payment \
  --market-ref 'a7bd532b8c570667685fb1014b7ed47372ddab3e30bfb567a01c239833f966f0#0' \
  --out-file 'paymentDatum.json'
```

The `market-ref` field must match the output reference of the corresponding Spot UTxO.

#### Calculate the required beacon names to burn

Spot UTxOs have two beacons: 
- Spot beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
spotBeaconName=$(cardano-aftermarket beacon-name spot-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --stdout)

spotBeacon="${beaconPolicyId}.${spotBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

##### Building the transaction

The sale price assets and the sale deposit must be output to the required payment address, and stored
with the proper payment datum.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/purchase-spot.sh).

## Creating an Auction UTxO

Creating an Auction UTxO involves the following steps:
1. Create your seller address.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required SpotDatum for each Auction UTxO you wish to create.
5. Submit a transaction that creates the Auction UTxOs.

#### Creating your seller address
```bash
# Export the spending script.
cardano-aftermarket scripts aftermarket-script \
  --out-file aftermarket.plutus

# Create the seller address.
cardano-cli address build \
  --payment-script-file aftermarket.plutus \
  --stake-verification-key-file seller_stake.vkey \
  --testnet-magic 1 \
  --out-file seller.addr
```

Cardano-Aftermarket also supports using staking scripts for the address. To use a staking script,
use the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

#### Calculate the required beacon names to mint

Auction UTxOs require two beacons: 
- Auction beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
auctionBeaconName=$(cardano-aftermarket beacon-name auction-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --stdout)

auctionBeacon="${beaconPolicyId}.${auctionBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the Auction Datum
```bash
cardano-aftermarket datums auction \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --nft-name '4f74686572546f6b656e0a' \
  --nft-name '54657374546f6b656e31' \
  --price '10000000 lovelace' \
  --price '10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000' \
  --out-file 'auctionDatum.json'
```

The `nft-policy-id` flag must match the policy id used for the Policy beacon.

You can specify as many `nft-name`s as you want; these are the NFTs (under the specified policy id)
that are for sale in this batch. *These fields must be adjacent to each other.*

The `price` fields make up the starting price for this auction. The bidder does not necessarily need
to offer more than this amount, and you can also accept a bid for less than this amount. The
starting price is just meant to convey the seller's desired price. The above example has the
starting price of:  
`10000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000`

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Building the transaction

Each Auction UTxO must be stored with the NFTs being auctioned in the batch.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-auction.sh).

## Closing an Auction UTxO

Closing an Auction UTxO involves the following steps:
1. Calculate the hash of the seller's staking credential.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required spending script redeemer.
5. Submit a transaction that closes the Auction UTxOs.

#### Calculate the hash of the staking credential used in the seller address
```bash
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file seller_stake.vkey)
```

#### Calculate the required beacon names to burn

Auction UTxOs have two beacons: 
- Auction beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
auctionBeaconName=$(cardano-aftermarket beacon-name auction-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --stdout)

auctionBeacon="${beaconPolicyId}.${auctionBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Create the required spending redeemer
```bash
cardano-aftermarket redeemers market-script manage-seller-utxo \
  --out-file 'closeOrUpdateSellerUTxO.json'
```

##### Building the transaction
The seller's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-auction.sh).

## Updating an Auction UTxO

The steps to update Auction UTxOs are exactly the same as closing them, except you will need to
create the new outputs. All redeemers are the same.

By cross-referencing the [creation script](scripts/local-node/create-auction.sh) and the [closing
script](scripts/local-node/close-auction.sh), you can easily come up with your own update script.

It is also possible to convert an Auction UTxO to a Spot UTxO.

## Creating a SpotBid UTxO

Creating a SpotBid UTxO involves the following steps:
1. Calculate the hash for the bidder credential.
1. Calculate the required beacon names to mint.
2. Create the required beacon script redeemer.
3. Create the required SpotBidDatum for each SpotBid UTxO you wish to create.
4. Submit a transaction that creates the SpotBid UTxOs.

#### Calculate the hash of the staking credential used in the BidderId
```bash
bidderCredentialHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file bidder_stake.vkey)
```

This must be the same credential used for the BidderId. If you are using a staking script, you can
create the hash with this:
```bash
bidderCredentialHash=$(cardano-cli transaction policyid \
  --script-file bidderStake.plutus)
```

#### Calculate the required beacon names to mint

SpotBid UTxOs require three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

If you are using a staking script to protect the SpotBid UTxOs, you can use `bidder-staking-script-hash`
when generating the BidderId name.

#### Create the Bid Datum
```bash
cardano-aftermarket datums spot-bid \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --nft-name '4f74686572546f6b656e0a' \
  --nft-name '54657374546f6b656e31' \
  --payment-address $(cat bidderPersonal.addr) \
  --bidder-deposit 5000000 \
  --price '10000000 lovelace' \
  --price '10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000' \
  --out-file 'bidDatum.json'
```

The `bidder-staking-pubkey-hash` is the credential used to protect the Bid UTxO. The BidderId must
use the same credential. If you are using a staking script instead for the credential, you can use
`bidder-staking-script-hash`. Make sure to properly designate if the credential is a script or
pubkey! You will need to witness the transaction with this credential.

The `nft-policy-id` flag must match the policy id used for the Policy beacon.

You can specify as many `nft-name`s as you want; these are the NFTs (under the specified policy id)
that you would like in exchange for the bid amount. *These fields must be adjacent to each other.*

The `payment-address` is where the payment must go when the bid is accepted by the seller.

The `bidder-deposit` is the amount of ada used for the minUTxOValue. This will be returned to you when the
bid is accepted.

The `price` fields make up the bid amount for this Bid UTxO. The above example has the bid of:  
`10000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000`

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Building the transaction

To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-spot-bid.sh).

## Closing a SpotBid UTxO

Closing a SpotBid UTxO involves the following steps:
1. Calculate the hash of the bidder's staking credential.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required spending script redeemer.
5. Submit a transaction that closes the SpotBid UTxOs.

#### Calculate the hash of the staking credential used in the BidderId
```bash
bidderCredentialHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file bidder_stake.vkey)
```

This must be the same credential used for the BidderId. If you are using a staking script, you can
create the hash with this:
```bash
bidderCredentialHash=$(cardano-cli transaction policyid \
  --script-file bidderStake.plutus)
```

#### Calculate the required beacon names to burn

SpotBid UTxOs have three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash $bidderCredentialHash \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Create the required spending redeemer
```bash
cardano-aftermarket redeemers market-script manage-bidder-utxo \
  --out-file 'closeOrUpdateBidderUTxO.json'
```

##### Building the transaction
The bidder's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-spot-bid.sh).

## Updating a SpotBid UTxO

The steps to update SpotBid UTxOs are exactly the same as closing them, except you will need to
create the new outputs. All redeemers are the same.

By cross-referencing the [creation script](scripts/local-node/create-spot-bid.sh) and the [closing
script](scripts/local-node/close-spot-bid.sh), you can easily come up with your own update script.

## Accepting a SpotBid UTxO

Accepting a SpotBid UTxO requires:

1. Create the required spending script redeemer.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required datum for the payments outputs.
5. Create the required staking address for the observer script.
6. Create the required observer script redeemer.
7. Submit the transaction.

If necessary, you can close the corresponding Auction UTxO in the same transaction, using the same
spending redeemer as the SpotBid UTxO being accepted.

#### Create the observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-aftermarket scripts \
  --observer-script \
  --out-file observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file observer.plutus)
```

#### Create the required redeemers
```bash
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file 'burnAllBeacons.json'

cardano-aftermarket redeemers market-script accept-spot-bid \
  --out-file 'acceptBid.json'

cardano-aftermarket redeemers observer-script observe \
  --out-file 'observe.json'
```

#### Create the payment datum
```bash
cardano-aftermarket datums payment \
  --market-ref 'a7bd532b8c570667685fb1014b7ed47372ddab3e30bfb567a01c239833f966f0#0' \
  --out-file 'paymentDatum.json'
```

The `market-ref` field must match the output reference of the corresponding Bid UTxO.

#### Calculate the required beacon names to burn

SpotBid UTxOs have two beacons: 
- Bid beacon
- BidderId
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

If you are closing the corresponding Auction UTxO as well, be sure to burn those beacons, too.

##### Building the transaction

The required NFTs and the bid deposit must be output to the required payment address, and stored
with the proper payment datum.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/accept-spot-bid.sh).

## Creating a ClaimBid UTxO

Creating a ClaimBid UTxO involves the following steps:
1. Calculate the bidder credential hash.
1. Calculate the required beacon names to mint.
2. Create the required beacon script redeemer.
3. Create the required ClaimBidDatum for each ClaimBid UTxO you wish to create.
4. Submit a transaction that creates the ClaimBid UTxOs.

#### Calculate the hash of the staking credential used in the BidderId
```bash
bidderCredentialHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file bidder_stake.vkey)
```

This must be the same credential used for the BidderId. If you are using a staking script, you can
create the hash with this:
```bash
bidderCredentialHash=$(cardano-cli transaction policyid \
  --script-file bidderStake.plutus)
```

#### Calculate the required beacon names to mint

ClaimBid UTxOs require three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash $bidderCredentialHash \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

If you are using a staking script to protect the ClaimBid UTxOs, you can use `bidder-staking-script-hash`
when generating the BidderId name.

#### Create the Bid Datum
```bash
cardano-aftermarket datums claim-bid \
  --bidder-staking-pubkey-hash $bidderCredentialHash \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --nft-name '4f74686572546f6b656e0a' \
  --nft-name '54657374546f6b656e31' \
  --bidder-deposit 5000000 \
  --price '10000000 lovelace' \
  --price '10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000' \
  --no-bid-expiration \
  --claim-expiration 1682351790000 \
  --out-file 'bidDatum.json'
```

The `bidder-staking-pubkey-hash` is the credential used to protect the Bid UTxO. The BidderId must
use the same credential. If you are using a staking script instead for the credential, you can use
`bidder-staking-script-hash`. Make sure to properly designate if the credential is a script or
pubkey! You will need to witness the transaction with this credential.

The `nft-policy-id` flag must match the policy id used for the Policy beacon.

You can specify as many `nft-name`s as you want; these are the NFTs (under the specified policy id)
that you would like in exchange for the bid amount. *These fields must be adjacent to each other.*

The `bidder-deposit` is the amount of ada used for the minUTxOValue. This will be returned to you when the
bid is accepted.

The `price` fields make up the bid amount for this Bid UTxO. The above example has the bid of:  
`10000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.000000`

The `no-bid-expiration` flag means the bid will not expire. If you would like to set an expiration,
use `bid-expiration` with a POSIXTime in milliseconds.

The `claim-expiration` sets the time when you will claim the NFTs by. It must be in POSIXTime in
milliseconds.

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Building the transaction

To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-claim-bid.sh).

## Closing a ClaimBid UTxO

Closing a ClaimBid UTxO involves the following steps:
1. Calculate the hash of the bidder's staking credential.
2. Calculate the required beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required spending script redeemer.
5. Submit a transaction that closes the ClaimBid UTxOs.

#### Calculate the hash of the staking credential used in the BidderId
```bash
bidderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $bidderStakePubKey)
```

#### Calculate the required beacon names to burn

ClaimBid UTxOs have three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

#### Create the required beacon redeemer
```bash
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file 'createCloseOrUpdateMarketUTxOs.json'
```

#### Create the required spending redeemer
```bash
cardano-aftermarket redeemers market-script manage-bidder-utxo \
  --out-file 'closeOrUpdateBidderUTxO.json'
```

##### Building the transaction
The bidder's staking credential must approve the transaction.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-claim-bid.sh).

## Updating a ClaimBid UTxO

The steps to update ClaimBid UTxOs are exactly the same as closing them, except you will need to
create the new outputs. All redeemers are the same.

By cross-referencing the [creation script](scripts/local-node/create-claim-bid.sh) and the [closing
script](scripts/local-node/close-claim-bid.sh), you can easily come up with your own update script.

## Accepting a ClaimBid UTxO

Accepting a ClaimBid UTxO requires:

1. Create the required spending script redeemer.
4. Create the required AcceptedBidDatum for the new bid output.
5. Create the required staking address for the observer script.
6. Create the required observer script redeemer.
7. Submit the transaction using invalid-hereafter if the ClaimBid expires.

If necessary, you can close the corresponding Auction UTxO in the same transaction, using the same
spending redeemer as the ClaimBid UTxO being accepted.

#### Create the observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-aftermarket scripts \
  --observer-script \
  --out-file observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file observer.plutus)
```

#### Create the required redeemers
```bash
cardano-aftermarket redeemers market-script accept-claim-bid \
  --seller-deposit 2000000 \
  --payment-address $(cat sellerPersonal.addr) \
  --out-file 'acceptBid.json'

cardano-aftermarket redeemers observer-script observe \
  --out-file 'observe.json'
```

#### Creating the AcceptedBidDatum

You can either create the datum by looking up the corresponding ClaimBid's UTxO, or manuall.

```bash
# Lookup ClaimBid UTxO.
cardano-aftermarket datums accepted-bid auto \
  --testnet \
  --bid-ref 'a7bd532b8c570667685fb1014b7ed47372ddab3e30bfb567a01c239833f966f0#0' \
  --seller-deposit 2000000 \
  --payment-address $(cat sellerPersonal.addr) \
  --out-file 'acceptedBidDatum.json'

# Create manually. The fields must exactly match the fields in the ClaimBidDatum.
cardano-aftermarket datums accepted-bid manual \
  --nft-policy-id 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d' \
  --bidder-staking-pubkey-hash "623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c" \
  --nft-name '4f74686572546f6b656e0a' \
  --nft-name '54657374546f6b656e31' \
  --bidder-deposit 5000000 \
  --seller-deposit 2000000 \
  --price '10000000 lovelace' \
  --claim-expiration 1727465638000 \
  --payment-address $(cat sellerPersonal.addr) \
  --out-file 'acceptedBidDatum.json'
```

##### Building the transaction
The seller's staking credential must approve the transaction. invalid-hereafter must be set to
the ClaimBid UTxO's `bid-expiration` if it is used.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/accept-claim-bid.sh).

## Claiming an AcceptedBid UTxO

Claiming an AcceptedBid UTxO requires:

1. Create the required spending script redeemer.
2. Creating the requried beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required datum for the payments outputs.
4. Create the required staking address for the observer script.
4. Create the required observer script redeemer.
5. Submit the transaction.

#### Create the observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-aftermarket scripts \
  --observer-script \
  --out-file observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file observer.plutus)
```

#### Create the required redeemers
```bash
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file 'burnAllBeacons.json'

cardano-aftermarket redeemers market-script claim-accepted-bid \
  --out-file 'claimAccepted.json'

cardano-aftermarket redeemers observer-script observe \
  --out-file 'observe.json'
```

#### Create the payment datum
```bash
cardano-aftermarket datums payment \
  --market-ref 'a7bd532b8c570667685fb1014b7ed47372ddab3e30bfb567a01c239833f966f0#0' \
  --out-file 'paymentDatum.json'
```

The `market-ref` field must match the output reference of the corresponding AcceptedBid UTxO.

#### Calculate the required beacon names to burn

AcceptedBid UTxOs have three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

If you are using a staking script to protect the SpotBid UTxOs, you can use `bidder-staking-script-hash`
when generating the BidderId name.

##### Building the transaction
The bidder credential used for the BidderId must witness the transaction. The bid asset + the
seller's deposit must be output to the required payment address, and stored with the proper payment
datum.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/claim-accepted-bid.sh).

## Unlocking an Unclaimed AcceptedBid UTxO

Unlocking an unclaimed AcceptedBid UTxO requires:

1. Create the required spending script redeemer.
2. Creating the requried beacon names to burn.
3. Create the required beacon script redeemer.
4. Create the required staking address for the observer script.
4. Create the required observer script redeemer.
5. Submit the transaction using invalid-before.

#### Create the observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-aftermarket scripts \
  --observer-script \
  --out-file observer.plutus

## Build the observer script's stake address.
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file observer.plutus)
```

#### Create the required redeemers
```bash
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file 'burnAllBeacons.json'

cardano-aftermarket redeemers market-script unlock \
  --out-file 'unlock.json'

cardano-aftermarket redeemers observer-script observe \
  --out-file 'observe.json'
```

#### Calculate the required beacon names to burn

AcceptedBid UTxOs have three beacons: 
- Bid beacon
- BidderId beacon
- Policy beacon

```bash
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash '623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c' \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"
```

##### Building the transaction
The seller must witness the transaction and invalid-before must be set to the AcceptedBid UTxO's
`claim-expiration`.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/unlock-unclaimed.sh).

## Time Conversions
Since plutus scripts use POSIX time (in milliseconds) while cardano-cli uses slot numbers for the
transaction validity intervals, you need a way to convert between the two units.

``` Bash
cardano-aftermarket convert-time --testnet --slot 26668590

cardano-aftermarket convert-time --testnet --posix-time 1682351790000
```

## Queries

- All queries use Koios.
- All query commands are capable of saving results to a file or printing to stdout. 
- Results can be formatted as JSON, pretty, or plain. 

The pretty and plain formats are meant for printing to the stdout, but both can also be saved to a
file. The only difference between the pretty format and the plain format is the pretty format uses
ansii escape sequences to highlight certain items with color. The plain format is there as a
fallback in case the ansii escape sequences are causing issues for a user.

The JSON response to stdout can be directly piped into `jq` for a more human-friendly format.

> Note: Currently, the `cardano-aftermarket` CLI will only get the first 1000 UTxOs that satisfy a
> query. This could be 1000 personal UTxOs or 1000 contract UTxOs, depending on the query. For the
> beta release, 1000 should be plenty. The CLI will be expanded in the future to remove this cap.

### Querying Personal Addresses

In order to facilitate the use of remote nodes, `cardano-aftermarket` is capable of querying personal
addresses.

The command is simply:
```bash
cardano-aftermarket query personal-address \
  --testnet \
  --address $(cat personal.addr) \
  --pretty \
  --stdout
```

The UTxOs will be returned in lexicographical order based on the transaction hash.

For the pretty and plain formats, UTxOs that contain a reference script and/or a datum will show the
script hash or datum hash, respectively. For the pretty format, each type of hash will have a color
associated with it: Blue for script hashes and Green for datum hashes. UTxO assets are always shown.

This query can also work on plutus script addresses.

### Querying Spot UTxOs

All possible queries for Spot UTxOs are organized under the `cardano-aftermarket query spots`
command.

*An nft policy id must be specified.* You can optionally specify a seller's staking credential to
query only the Spot UTxOs that belong to that seller.

### Querying Auction UTxOs

All possible queries for Auction UTxOs are organized under the `cardano-aftermarket query auctions`
command.

*An nft policy id must be specified.* You can optionally specify a seller's staking credential to
query only the Auction UTxOs that belong to that seller.

### Querying Bid UTxOs

All possible queries for Bid UTxOs are organized under the `cardano-aftermarket query bids`
command. This command will return all bids, regardless of the type of bid. AcceptedBids are also
returned by this query.

*An nft policy id must be specified.* You can optionally specify a seller's staking credential to
query the Bid UTxOs that belong to that seller, and/or a bidder's staking credential to query Bids
that belong to that bidder.
