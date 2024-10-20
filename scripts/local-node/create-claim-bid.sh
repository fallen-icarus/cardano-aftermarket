#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

sellerAddr="addr_test1zrs8a6yhjamxjt8rgaascr2nknr9pmmvett4cfvkmg3gglpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aquyamsh"

bidderStakePubKey="${walletDir}02Stake.vkey"
bidderPersonalAddr="$(cat ${walletDir}02.addr)"

bidDatumFile="${filesDir}bidDatum.json"
beaconRedeemerFile="${filesDir}createCloseOrUpdateMarketUTxOs.json"

bidDeposit=5000000 # 5 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

claimExpirationSlot=$((71778838 + 3600))
claimExpirationTime=$(cardano-aftermarket convert-time --slot $claimExpirationSlot --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the bidder..."
bidderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $bidderStakePubKey)

## Create the bid datum.
echo "Creating the bid datum..."
cardano-aftermarket datums claim-bid \
  --bidder-staking-pubkey-hash $bidderStakePubKeyHash \
  --nft-policy-id $nftPolicyId \
  --nft-name $nftName1 \
  --nft-name $nftName2 \
  --bidder-deposit $bidDeposit \
  --price '10000000 lovelace' \
  --no-bid-expiration \
  --claim-expiration $claimExpirationTime \
  --out-file $bidDatumFile

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
### Do NOT store the bid amount in the ClaimBid UTxO!
cardano-cli conway transaction build \
  --tx-in 727cc3a038ff4d095f76b95aadf5be07c04524d29f2f12d2f320d46641abfda8#1 \
  --tx-out "${sellerAddr} + ${bidDeposit} lovelace + 1 ${bidBeacon} + 1 ${policyBeacon} + 1 ${bidderId}" \
  --tx-out-inline-datum-file $bidDatumFile \
  --mint "1 ${bidBeacon} + 1 ${policyBeacon} + 1 ${bidderId}" \
  --mint-tx-in-reference e7b3cd91a7127607fb2fddf2efe78559321175c8660e47350bc5f8e91a90c4d5#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $bidderStakePubKeyHash \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --invalid-hereafter $claimExpirationSlot \
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
