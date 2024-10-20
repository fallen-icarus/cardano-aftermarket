#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

scriptFile="${filesDir}beacons.plutus"

## Export the script.
echo "Exporting the beacon script..."
cardano-aftermarket scripts \
  --beacon-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --tx-in ccf77ee5926704fbbbf7408ff82e11ed3270696b407c6ca9bef03a757d439a93#0 \
  --tx-out "$(cat "${walletDir}01.addr") + 36000000 lovelace" \
  --tx-out-reference-script-file $scriptFile \
  --change-address "$(cat "${walletDir}01.addr")" \
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
