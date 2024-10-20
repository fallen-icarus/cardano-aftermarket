#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

beaconScript="${filesDir}beacons.plutus"
observerScript="${filesDir}observer.plutus"

beaconCert="${filesDir}beacons.cert"
observerCert="${filesDir}observer.cert"

beaconRedeemer="${filesDir}beacons_redeemer.json"
observerRedeemer="${filesDir}observer_redeemer.json"

## Export the scripts.
echo "Exporting the scripts..."
cardano-aftermarket scripts \
  --beacon-script \
  --out-file $beaconScript

cardano-aftermarket scripts \
  --observer-script \
  --out-file $observerScript

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $beaconScript \
  --out-file $beaconCert

cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $observerScript \
  --out-file $observerCert

echo "Exporting the redeemers..."
cardano-aftermarket redeemers beacon-script register \
  --out-file $beaconRedeemer

cardano-aftermarket redeemers observer-script register \
  --out-file $observerRedeemer

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $beaconCert \
  --certificate-tx-in-reference e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-file $observerCert \
  --certificate-tx-in-reference 7ede060e563b49e36ea0efe17d93bb83353d560f18c8b6fdaa696475babe6575#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $observerRedeemer \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-aftermarket submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
