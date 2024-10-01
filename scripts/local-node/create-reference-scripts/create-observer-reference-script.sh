#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

scriptFile="${filesDir}observer.plutus"

## Export the script.
echo "Exporting the market observer script..."
cardano-aftermarket scripts \
  --observer-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 193101ee9a137a77ca6ebb7f279f63d27b9cc7c2f9f348fbf7b5324b3d3c4634#0 \
  --tx-in 1cf071ff8dd90288de651fd6b774f1e6cc4959faf52cb1261b9b429b1710f8f1#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 44000000 lovelace" \
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
