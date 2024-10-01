#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

observerScript="${filesDir}observer.plutus"

paymentDatumFile="${filesDir}paymentDatum.json"

bidderStakePubKey="${walletDir}02Stake.vkey"

paymentAddress="addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f"

beaconRedeemerFile="${filesDir}burnAllBeacons.json"
marketRedeemerFile="${filesDir}acceptBid.json"
observerRedeemerFile="${filesDir}observePayment.json"

bidRef="8e543433e93f01a5cce45d0c441d69035865851afab78a9f740142c1a441bf6c#0"

sellerDeposit=0 # 0 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the bidder..."
bidderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $bidderStakePubKey)

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
cardano-aftermarket redeemers market-script claim-accepted-bid \
  --out-file $marketRedeemerFile

echo "Creating the observer redeemer..."
cardano-aftermarket redeemers observer-script observe \
  --out-file $observerRedeemerFile

## Create the payment datum.
echo "Creating the payment datum..."
cardano-aftermarket datums payment \
  --market-ref $bidRef \
  --out-file $paymentDatumFile

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
  --bidder-staking-pubkey-hash $bidderStakePubKeyHash \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
### The seller must be paid their deposit + the bid amount.
cardano-cli conway transaction build \
  --tx-in e01235cd8ad6991f8bc3abe2b4d1c288897bb159f98750647d468df8189cdd6f#1 \
  --tx-in $bidRef \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "${paymentAddress} + ${sellerDeposit} lovelace + 10000000 lovelace" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "$(cat ${walletDir}02.addr) + 3000000 lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --mint "-1 ${bidBeacon} + -1 ${policyBeacon} + -1 ${bidderId}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 193101ee9a137a77ca6ebb7f279f63d27b9cc7c2f9f348fbf7b5324b3d3c4634#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $bidderStakePubKeyHash \
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
