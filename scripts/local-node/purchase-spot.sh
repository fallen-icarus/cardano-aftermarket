#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

observerScript="${filesDir}observer.plutus"
paymentDatumFile="${filesDir}paymentDatum.json"

paymentAddr="addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f"

beaconRedeemerFile="${filesDir}burnAllBeacons.json"
marketRedeemerFile="${filesDir}purchaseSpot.json"
observerRedeemerFile="${filesDir}observe.json"

spotRef="a51a7e8166a5d59ba1f93465ee6466c26e88e80af35bb997b6e3cd86a8d410d1#0"

saleDeposit=5000000 # 5 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Export the payment observer script so you can generate its required stake address.
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
cardano-aftermarket redeemers market-script purchase-spot \
  --out-file $marketRedeemerFile

echo "Creating the observer redeemer..."
cardano-aftermarket redeemers observer-script observe \
  --out-file $observerRedeemerFile

## Create the payment datum.
echo "Creating the payment datum..."
cardano-aftermarket datums payment \
  --market-ref $spotRef \
  --out-file $paymentDatumFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 

## Get the required beacon names.
spotBeaconName=$(cardano-aftermarket beacon-name spot-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)

spotBeacon="${beaconPolicyId}.${spotBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 292ccf2bc14d685258bf0d3f070d4319eca74889269c962451ccb1ef6462084d#2 \
  --tx-in dabfb2819e70b22485eccacfc838b5decbcba2df9b5d19afe5cd0b90bc6e8679#1 \
  --tx-in $spotRef \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "${paymentAddr} + ${saleDeposit} lovelace + 10000000 lovelace" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "$(cat ${walletDir}02.addr) + 3000000 lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --mint "-1 ${spotBeacon} + -1 ${policyBeacon}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 193101ee9a137a77ca6ebb7f279f63d27b9cc7c2f9f348fbf7b5324b3d3c4634#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --signing-key-file "${walletDir}/02Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-aftermarket submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
