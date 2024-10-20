#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

sellerStakePubKey="${walletDir}01Stake.vkey"

beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"
marketRedeemerFile="${filesDir}closeOrUpdateSellerUTxO.json"

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the seller..."
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $sellerStakePubKey)

## Create the required redeemers.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script manage-market-utxos \
  --out-file $beaconRedeemerFile

echo "Creating the market redeemer..."
cardano-aftermarket redeemers market-script manage-seller-utxo \
  --out-file $marketRedeemerFile

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
  --tx-in b9d98aa9d66df40506725fbd548052a638f79656d97b39ba2de8d924e4cd0a06#1 \
  --tx-in 420e13c0d0823666de7828c703c537c7e4b2bdaf6f079c93a744e7b026cb3f92#0 \
  --spending-tx-in-reference cb9675e9f74b559aa2f9196e0a985debf93621f38e24be6711f9872d40ee06ea#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --mint "-1 ${spotBeacon} + -1 ${policyBeacon}" \
  --mint-tx-in-reference d725ebf0adcc114251b993389032e4d67bfbdcaeaeb368edf540877d86562167#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
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
