#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

observerScript="${filesDir}observer.plutus"

sellerStakePubKey="${walletDir}01Stake.vkey"
bidderCredential="623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"

beaconRedeemerFile="${filesDir}burnBeacons.json"
marketRedeemerFile="${filesDir}unlock.json"
observerRedeemerFile="${filesDir}observePayment.json"

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

# This must be used for invalid-before.
claimExpirationSlot=71782438

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

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the seller..."
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $sellerStakePubKey)

## Create the required redeemers.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file $beaconRedeemerFile

echo "Creating the market redeemer..."
cardano-aftermarket redeemers market-script unlock \
  --out-file $marketRedeemerFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 

## Get the required beacon names.
bidBeaconName=$(cardano-aftermarket beacon-name bid-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)
bidderIdName=$(cardano-aftermarket beacon-name bidder-id \
  --bidder-staking-pubkey-hash $bidderCredential \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 2e5f567563d54f23ccb9a5fe4ae72b57b8a131addea3678f5f023536611f546a#0 \
  --spending-tx-in-reference 41aa389294c7dd41edfe993df3e2224c70e88212888cfdcbcea6529f5abd3a9e#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --mint "-1 ${bidBeacon} + -1 ${policyBeacon} + -1 ${bidderId}" \
  --mint-tx-in-reference e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 7ede060e563b49e36ea0efe17d93bb83353d560f18c8b6fdaa696475babe6575#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $sellerStakePubKeyHash \
  --invalid-before $claimExpirationSlot \
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
