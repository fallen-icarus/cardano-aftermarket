alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName=$(echo -n "OtherToken" | xxd -ps)
tmpDir="../../../ignored/tmp/"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli conway transaction build \
  --tx-in 37acb503d196d3e9c0f5545747b22efa3454b61d8ff6cdb83927f91be9d5e864#2 \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address $(cat ../../../ignored/wallets/01.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
