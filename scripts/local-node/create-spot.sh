#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

marketScript="${filesDir}aftermarket.plutus"

sellerStakePubKey="${walletDir}01Stake.vkey"
sellerPersonalAddr="$(cat ${walletDir}01.addr)"

spotDatumFile="${filesDir}spotDatum.json"
beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"

saleDeposit=5000000 # 5 ADA

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

## Create the spot datum.
echo "Creating the spot datum..."
cardano-aftermarket datums spot \
  --nft-policy-id $nftPolicyId \
  --nft-name $nftName1 \
  --nft-name $nftName2 \
  --payment-address "$sellerPersonalAddr" \
  --seller-deposit $saleDeposit \
  --price '10000000 lovelace' \
  --out-file $spotDatumFile

## Create the required redeemer.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file $beaconRedeemerFile

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
  --tx-in c543a5bd46d6e0edfc145f97641100b36d40be15d19c669f07f2ccb2bd76a0fd#4 \
  --tx-out "${sellerAddr} + ${saleDeposit} lovelace + 1 ${spotBeacon} + 1 ${policyBeacon} + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $spotDatumFile \
  --mint "1 ${spotBeacon} + 1 ${policyBeacon}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
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
