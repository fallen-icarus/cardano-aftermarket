#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

observerScript="${filesDir}observer.plutus"

acceptedDatumFile="${filesDir}acceptedDatum.json"

sellerStakePubKey="${walletDir}01Stake.vkey"
marketAddr="addr_test1zrs8a6yhjamxjt8rgaascr2nknr9pmmvett4cfvkmg3gglpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aquyamsh"

bidderCredentialHash="623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"

beaconRedeemerFile="${filesDir}burnAllBeacons.json"
marketRedeemerFile="${filesDir}acceptBid.json"
observerRedeemerFile="${filesDir}observePayment.json"

bidRef="d967c1bcbdffbf6678dd4d627d2047e67f197809abc2e78f1c821345e25ed20c#0"
auctionRef="37acb503d196d3e9c0f5545747b22efa3454b61d8ff6cdb83927f91be9d5e864#0"

bidDeposit=5000000 # 5 ADA
sellerDeposit=0 # 0 ADA

paymentAddress="$(cat ${walletDir}01.addr)"

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

# If the bid expires, you must use it for invalid-hereafter.
expirationSlot=71782438

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the seller..."
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $sellerStakePubKey)

## Export the observer script so you can generate its required stake address.
echo "Exporting the observer script..."
cardano-aftermarket scripts \
  --observer-script \
  --out-file $observerScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $observerScript)

## Create the required redeemers.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file $beaconRedeemerFile

echo "Creating the market redeemer..."
cardano-aftermarket redeemers market-script accept-claim-bid \
  --seller-deposit $sellerDeposit \
  --payment-address $paymentAddress \
  --out-file $marketRedeemerFile

echo "Creating the observer redeemer..."
cardano-aftermarket redeemers observer-script observe \
  --out-file $observerRedeemerFile

## Create the accepted bid datum.
echo "Creating the new accepted bid datum..."
cardano-aftermarket datums accepted-bid auto \
  --testnet \
  --bid-ref $bidRef \
  --seller-deposit $sellerDeposit \
  --payment-address $paymentAddress \
  --out-file $acceptedDatumFile

# cardano-aftermarket datums accepted-bid manual \
#   --nft-policy-id $nftPolicyId \
#   --bidder-staking-pubkey-hash $bidderCredentialHash \
#   --nft-name $nftName1 \
#   --nft-name $nftName2 \
#   --bidder-deposit $bidDeposit \
#   --seller-deposit $sellerDeposit \
#   --price '10000000 lovelace' \
#   --claim-expiration 1727465638000 \
#   --payment-address $paymentAddress \
#   --out-file $acceptedDatumFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 

## Get the required beacon names.
auctionBeaconName=$(cardano-aftermarket beacon-name auction-beacon \
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
auctionBeacon="${beaconPolicyId}.${auctionBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 6a1a0d9636e6838e8cad23652ace000a134f66862b9b175fb9c00dcc3406c194#1 \
  --tx-in $bidRef \
  --spending-tx-in-reference 41aa389294c7dd41edfe993df3e2224c70e88212888cfdcbcea6529f5abd3a9e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-in $auctionRef \
  --spending-tx-in-reference 41aa389294c7dd41edfe993df3e2224c70e88212888cfdcbcea6529f5abd3a9e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "${marketAddr} + ${bidDeposit} lovelace + ${sellerDeposit} lovelace + 1 ${bidBeacon} + 1 ${policyBeacon} + 1 ${bidderId} + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $acceptedDatumFile \
  --mint "-1 ${policyBeacon} + -1 ${auctionBeacon}" \
  --mint-tx-in-reference e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 7ede060e563b49e36ea0efe17d93bb83353d560f18c8b6fdaa696475babe6575#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --invalid-hereafter $expirationSlot \
  --required-signer-hash $sellerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-aftermarket submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
