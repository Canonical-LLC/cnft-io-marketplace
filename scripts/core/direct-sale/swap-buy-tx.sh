set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

bodyFile=temp/swap-tx-body.01
outFile=temp/swap-tx.01
buyerAddr=$1
sellerAddr=$2
marketplaceAddr=$3
signingKey=$4
value=$5
sellerAmount=$6
marketPlaceAmount=$7
datumFile="${8}"
royalitiesAddr="${9}"
royalitiesAmount="${10}"
redeemerFile="${11}"
datumHash="${12}"
spenderAddress="${13}"
payinAddress="${14}"
payinSigningKey="${15}"
subtractOutput="${16:- 0 lovelace}"
nftValidatorFile=$baseDir/direct-sale.plutus
scriptHash=$(cat scripts/$BLOCKCHAIN_PREFIX/direct-sale.addr)

utxoScript=$(scripts/query/direct-sale.sh | grep $datumHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
changeOutput=$(cardano-cli-balance-fixer change --address $spenderAddress -o "$subtractOutput" $BLOCKCHAIN)

activityToken="$(cat $baseDir/test-policies/test-policy-0-id.txt).434e4654494f"

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

changeOutput1=$(cardano-cli-balance-fixer change --address $payinAddress -o "2 $activityToken" $BLOCKCHAIN)
extraOutput1=""
if [ "$changeOutput1" != "" ];then
  extraOutput1="+ $changeOutput1"
fi


currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $spenderAddress $BLOCKCHAIN ) \
    $(cardano-cli-balance-fixer input --address $payinAddress $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --required-signer $payinSigningKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $spenderAddress $BLOCKCHAIN ) \
    --tx-out "$sellerAddr + $sellerAmount + 1 $activityToken" \
    --tx-out "$buyerAddr + $value + 1 $activityToken" \
    --tx-out "$marketplaceAddr + $marketPlaceAmount" \
    --tx-out "$royalitiesAddr + $royalitiesAmount" \
    --tx-out "$spenderAddress + 3000000 lovelace $extraOutput" \
    --tx-out "$payinAddress + 3000000 lovelace $extraOutput1" \
    --change-address $spenderAddress \
    --protocol-params-file $baseDir/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --invalid-hereafter $nextTenSlots \
    --out-file $bodyFile

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   --signing-key-file $payinSigningKey \
   $BLOCKCHAIN \
   --out-file $outFile

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile
