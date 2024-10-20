#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

observerScript="${filesDir}observer.plutus"

paymentDatumFile="${filesDir}paymentDatum.json"

sellerStakePubKey="${walletDir}01Stake.vkey"

bidderCredentialHash="623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"
paymentAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

beaconRedeemerFile="${filesDir}burnAllBeacons.json"
marketRedeemerFile="${filesDir}acceptBid.json"
observerRedeemerFile="${filesDir}observePayment.json"

bidRef="8a0a41149efa11d1fa8399541b3d112b39720005603939d40e0a7b638da6ec30#0"
auctionRef="f2ef0c2b588d0c46d3481510f695e745a7b142dcdd58810d5cd89ee0d1c20e02#0"

bidDeposit=5000000 # 5 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

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
cardano-aftermarket redeemers market-script accept-spot-bid \
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
  --tx-in $bidRef \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-in $auctionRef \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "${paymentAddress} + ${bidDeposit} lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --mint "-1 ${bidBeacon} + -2 ${policyBeacon} + -1 ${bidderId} + -1 ${auctionBeacon}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 193101ee9a137a77ca6ebb7f279f63d27b9cc7c2f9f348fbf7b5324b3d3c4634#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
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
