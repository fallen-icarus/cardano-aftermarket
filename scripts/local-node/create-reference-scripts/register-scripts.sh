#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

beaconScript="${filesDir}beacons.plutus"
paymentScript="${filesDir}payment_observer.plutus"

beaconCert="${tmpDir}beacons.cert"
paymentCert="${tmpDir}payment_observer.cert"

## Export the scripts.
echo "Exporting the scripts..."
cardano-aftermarket scripts \
  --beacon-script \
  --out-file $beaconScript

cardano-aftermarket scripts \
  --payment-script \
  --out-file $paymentScript

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli stake-address registration-certificate \
  --stake-script-file $beaconScript \
  --out-file $beaconCert

cardano-cli stake-address registration-certificate \
  --stake-script-file $paymentScript \
  --out-file $paymentCert

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in b4fbe31ad2af2da42ede40d02585c3275c449e1f4caab8d3627069c7403c771d#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $beaconCert \
  --certificate-file $paymentCert \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
