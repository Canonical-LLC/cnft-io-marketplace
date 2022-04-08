set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
receiverAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
value="+ 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"

changeOutput=$(cardano-cli-balance-fixer change --address $senderAddr $BLOCKCHAIN -o "$value")

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  $(cardano-cli-balance-fixer input --address $senderAddr $BLOCKCHAIN ) \
  --tx-out "$receiverAddr +1724100 lovelace $value" \
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
