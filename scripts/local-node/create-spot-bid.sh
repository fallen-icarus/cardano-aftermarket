#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

sellerAddr="addr_test1zq4qv77tyfwzj8agpdhyvetj22r2zqv6pwspv4yqpaxul6fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqnm2x9q"

bidderStakePubKey="${walletDir}02Stake.vkey"
bidderPersonalAddr="$(cat ${walletDir}02.addr)"

bidDatumFile="${filesDir}bidDatum.json"
beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"

bidDeposit=5000000 # 5 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the bidder..."
bidderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $bidderStakePubKey)

## Create the bid datum.
echo "Creating the bid datum..."
cardano-aftermarket datums spot-bid \
  --bidder-staking-pubkey-hash $bidderStakePubKeyHash \
  --nft-policy-id $nftPolicyId \
  --nft-name $nftName1 \
  --nft-name $nftName2 \
  --payment-address "$bidderPersonalAddr" \
  --bidder-deposit $bidDeposit \
  --price '10000000 lovelace' \
  --out-file $bidDatumFile

## Create the required redeemer.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file $beaconRedeemerFile

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
  --tx-in b0c01d20f93919a725c2382cc7f0386f8cb76724d10c72f2ee672d12c9d0f2ec#0 \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#1 \
  --tx-out "${sellerAddr} + ${bidDeposit} lovelace + 1 ${bidBeacon} + 1 ${policyBeacon} + 1 ${bidderId} + 10000000 lovelace" \
  --tx-out-inline-datum-file $bidDatumFile \
  --mint "1 ${bidBeacon} + 1 ${policyBeacon} + 1 ${bidderId}" \
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
