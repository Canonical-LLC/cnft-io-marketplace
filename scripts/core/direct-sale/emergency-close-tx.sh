set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp



bodyFile=$tempDir/cancel-tx-body.01
outFile=$tempDir/cancel-tx.01
walletAddr=$1
signingKey=$2
value=$3
datumFile=$4
datumHash=$5
outputAddr=$6

utxoScript=$(scripts/query/direct-sale.sh | grep $datumHash  | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/direct-sale.addr)
nftValidatorFile=$baseDir/direct-sale.plutus
redeemerFile=scripts/redeemers/direct-sale/emergency-close.json

changeOutput=$(cardano-cli-balance-fixer change --address $walletAddr $BLOCKCHAIN)
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $walletAddr $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $walletAddr $BLOCKCHAIN) \
    --tx-out "$outputAddr + $value" \
    --tx-out "$walletAddr + 2344776 lovelace $extraOutput" \
    --change-address $walletAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --invalid-before $startSlot\
    --invalid-hereafter $nextTenSlots \
    --out-file $bodyFile

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile
