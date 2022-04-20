set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
receiverAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)

changeOutput=$(cardano-cli-balance-fixer change --address $senderAddr $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  $(cardano-cli-balance-fixer input --address $senderAddr $BLOCKCHAIN ) \
  --tx-out "$receiverAddr +100000000 lovelace" \
  --tx-out "$senderAddr + 1724100 lovelace $extraOutput" \
  --change-address $senderAddr \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"
