set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

buyerAddr=$1
signingKey=$2
value=$3
oldDatumFile=$4
oldDatumHash=$5
newDatumFile=$6
newDatumHash=$7
escrowDatumFile=$8
escrowHash=${9}
bidAmount=${10}

redeemerFile=$baseDir/redeemers/collect.json
nftValidatorFile=$baseDir/auction.plutus
escrowScriptFile=$baseDir/escrow.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)
escrowAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/escrow.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01

utxoScript=$(scripts/query/auction.sh | grep $oldDatumHash | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
escrowScriptUtxo=$(scripts/query/escrow.sh | grep $escrowHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
escrowUnlockRedeemer=$baseDir/redeemers/unlock.json

output1=$(cardano-cli-balance-fixer utxo-assets --utxo $utxoScript $BLOCKCHAIN)
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))
changeOutput=$(cardano-cli-balance-fixer change --address $buyerAddr $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $buyerAddr $BLOCKCHAIN ) \
    --tx-in $escrowScriptUtxo \
    --tx-in-script-file $escrowScriptFile \
    --tx-in-datum-file $escrowDatumFile \
    --tx-in-redeemer-file $escrowUnlockRedeemer \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $oldDatumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $buyerAddr $BLOCKCHAIN) \
    --tx-out "$scriptHash + $output1 + $bidAmount lovelace" \
    --tx-out-datum-hash $newDatumHash \
    --tx-out-datum-embed-file $newDatumFile \
    --tx-out "$buyerAddr + 3000000 lovelace $extraOutput" \
    --change-address $buyerAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --invalid-before $startSlot\
    --invalid-hereafter $nextTenSlots \
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

echo
