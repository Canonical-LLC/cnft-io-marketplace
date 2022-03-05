set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/buyer1.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
buyer1Addr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 8340d2e5979729d006d836ca8eeca2395c73770529775139d2126d1fc68ed145#1 \
  --tx-in 8340d2e5979729d006d836ca8eeca2395c73770529775139d2126d1fc68ed145#0 \
  --tx-out "$buyerAddr +100000000 lovelace" \
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
