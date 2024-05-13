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
cardano-cli transaction build \
  --tx-in ebdd93fc4f43da10a3b9900adc79884204b62cbc0bcc11a4ba9805ba3039a36a#1 \
  --tx-in ebdd93fc4f43da10a3b9900adc79884204b62cbc0bcc11a4ba9805ba3039a36a#0 \
  --tx-out "${sellerAddr} + 3000000 lovelace + 1 ${auctionBeacon} + 1 ${policyBeacon} + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $auctionDatumFile \
  --mint "1 ${auctionBeacon} + 1 ${policyBeacon}" \
  --mint-tx-in-reference e6573f7aef914f388bf792e28fdb6af3d2acde3e7b1070484044fb84bee3fa6d#0 \
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

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
