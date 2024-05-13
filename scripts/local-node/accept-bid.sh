#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
filesDir="${mainDir}files/"
tmpDir="${mainDir}tmp/"

paymentObserverScript="${filesDir}paymentObserver.plutus"

paymentDatumFile="${filesDir}paymentDatum.json"

sellerStakePubKey="${walletDir}01Stake.vkey"

bidderCredentialHash="623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"
paymentAddr="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

beaconRedeemerFile="${filesDir}burnAllBeacons.json"
marketRedeemerFile="${filesDir}acceptBid.json"
observerRedeemerFile="${filesDir}observePayment.json"

bidRef="d16fab2eeea29a0487b5fde4b1c971e8b15ca710c3e6543a33d4ec5726a1fdbd#0"
auctionRef="7a84bc049ca8f10fe016bcf4a79aa15ca01117218a5cd721123257c395832b1a#0"

bidDeposit=5000000 # 5 ADA

nftPolicyId='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d'
nftName1='4f74686572546f6b656e0a'
nftName2='54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the seller..."
sellerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $sellerStakePubKey)

## Export the payment observer script so you can generate its required stake address.
echo "Exporting the payment observer script..."
cardano-aftermarket scripts \
  --payment-script \
  --out-file $paymentObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $paymentObserverScript)

## Create the required redeemers.
echo "Creating the beacon redeemer..."
cardano-aftermarket redeemers beacon-script burn-all \
  --out-file $beaconRedeemerFile

echo "Creating the market redeemer..."
cardano-aftermarket redeemers market-script accept-bid \
  --out-file $marketRedeemerFile

echo "Creating the observer redeemer..."
cardano-aftermarket redeemers payment-script observe-payment \
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
cardano-cli transaction build \
  --tx-in $bidRef \
  --spending-tx-in-reference b4fbe31ad2af2da42ede40d02585c3275c449e1f4caab8d3627069c7403c771d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-in $auctionRef \
  --spending-tx-in-reference b4fbe31ad2af2da42ede40d02585c3275c449e1f4caab8d3627069c7403c771d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $marketRedeemerFile \
  --tx-out "${paymentAddr} + ${bidDeposit} lovelace + 1 ${nftPolicyId}.${nftName1} + 1 ${nftPolicyId}.${nftName2}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --mint "-1 ${bidBeacon} + -2 ${policyBeacon} + -1 ${bidderId} + -1 ${auctionBeacon}" \
  --mint-tx-in-reference e6573f7aef914f388bf792e28fdb6af3d2acde3e7b1070484044fb84bee3fa6d#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 6c87bb937fd938a4dd6dc65e0bc0740c55b1acad331740922b0f865a08cb54e8#0 \
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

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
