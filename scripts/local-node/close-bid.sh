#!/bin/sh

# This can be used to close both SpotBids and ClaimBids. This cannot be used to claim AcceptedBids!

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

bidderStakePubKey="${walletDir}02Stake.vkey"

beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"
marketRedeemerFile="${filesDir}closeOrUpdateBidderUTxO.json"

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the bidder..."
bidderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $bidderStakePubKey)

## Create the required redeemers.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file $beaconRedeemerFile

echo "Creating the market redeemer..."
cardano-aftermarket redeemers market-script manage-bidder-utxo \
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
  --bidder-staking-pubkey-hash $bidderStakePubKeyHash \
  --stdout)

bidBeacon="${beaconPolicyId}.${bidBeaconName}"
bidderId="${beaconPolicyId}.${bidderIdName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 12f04c7757876d28eccd1cc5d451a9250fe29cd3067b580b1556667eff009168#0 \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --mint "-1 ${bidBeacon} + -1 ${policyBeacon} + -1 ${bidderId}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
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
