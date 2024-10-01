#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

marketScript="${filesDir}aftermarket.plutus"

sellerStakePubKey="${walletDir}01Stake.vkey"

auctionDatumFile="${filesDir}auctionDatum.json"
beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Export the scripts.
echo "Exporting the scripts..."
cardano-aftermarket scripts \
  --market-script \
  --out-file $marketScript

## Create the market address.
echo "Creating the seller's market address..."
sellerAddr=$(cardano-cli address build \
  --payment-script-file $marketScript \
  --stake-verification-key-file $sellerStakePubKey \
  --testnet-magic 1)

## Create the auction datum.
echo "Creating the auction datum..."
cardano-aftermarket datums auction \
  --nft-policy-id $nftPolicyId \
  --nft-name $nftName1 \
  --nft-name $nftName2 \
  --price '10000000 lovelace' \
  --out-file $auctionDatumFile

## Create the required redeemer.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-aftermarket beacon-name policy-id \
  --stdout) 

## Get the required beacon names.
auctionBeaconName=$(cardano-aftermarket beacon-name auction-beacon \
  --stdout)
policyBeaconName=$(cardano-aftermarket beacon-name policy-beacon \
  --nft-policy-id $nftPolicyId \
  --stdout)

auctionBeacon="${beaconPolicyId}.${auctionBeaconName}"
policyBeacon="${beaconPolicyId}.${policyBeaconName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in b57c31eab87b3caf8be60860069f3e3275d79a9c28339a8af12623f1cd493de3#0 \
  --tx-in a51a7e8166a5d59ba1f93465ee6466c26e88e80af35bb997b6e3cd86a8d410d1#1 \
  --tx-out "${sellerAddr} + 3000000 lovelace + 1 ${auctionBeacon} + 1 ${policyBeacon} + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $auctionDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 11 ${nftPolicyId}.${nftName1} + 10 ${nftPolicyId}.${nftName2}" \
  --mint "1 ${auctionBeacon} + 1 ${policyBeacon}" \
  --mint-tx-in-reference e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --change-address "$(cat ${walletDir}01.addr)" \
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
