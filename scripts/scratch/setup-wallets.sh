set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=/Users/jonathanfischoff/prototypes/cardano-private-testnet-setup/private-testnet/addresses/user1.skey
senderAddr=$(cat /Users/jonathanfischoff/prototypes/cardano-private-testnet-setup/private-testnet/addresses/user1.addr)
outFile=temp/consolidate-tx.01
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
buyer1Addr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)
royalitiesAddr=$(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr)
attackerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/attacker.addr)
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)



cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  $(cardano-cli-balance-fixer input --address $senderAddr $BLOCKCHAIN ) \
  --tx-out "$sellerAddr + 45000000000 lovelace" \
  --tx-out "$buyerAddr + 45000000000 lovelace" \
  --tx-out "$buyer1Addr + 45000000000 lovelace" \
  --tx-out "$marketplaceAddr + 45000000000 lovelace" \
  --tx-out "$royalitiesAddr + 45000000000 lovelace" \
  --tx-out "$attackerAddr + 45000000000 lovelace" \
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
