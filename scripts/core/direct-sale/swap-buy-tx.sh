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
buyerExchangerDatum="${14}"
subtractOutput="${15:- 0 lovelace}"
nftValidatorFile=$baseDir/direct-sale.plutus
scriptHash=$(cat scripts/$BLOCKCHAIN_PREFIX/direct-sale.addr)

utxoScript=$(scripts/query/direct-sale.sh | grep $datumHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
changeOutput=$(cardano-cli-balance-fixer change --address $spenderAddress -o "$subtractOutput" $BLOCKCHAIN)

activityToken="$(cat $baseDir/activity-minter-hash.txt).4143544956495459"
mintValue="2 $activityToken"
activityMinterFile=$baseDir/activity-minter.plutus
mintActivityTokenFile=$baseDir/redeemers/mint.json

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

exchanger=$(cat $baseDir/$BLOCKCHAIN_PREFIX/exchanger.addr)
sellerExchangerDatum=$tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/sellerExchange.json

currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $spenderAddress $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $spenderAddress $BLOCKCHAIN ) \
    --tx-out "$sellerAddr + $sellerAmount" \
    --tx-out "$buyerAddr + $value" \
    --tx-out "$marketplaceAddr + $marketPlaceAmount" \
    --tx-out "$royalitiesAddr + $royalitiesAmount" \
    --tx-out "$spenderAddress + 3000000 lovelace $extraOutput" \
    --tx-out "$exchanger + 2000000 lovelace + 1 $activityToken" \
    --tx-out-datum-embed-file $sellerExchangerDatum \
    --tx-out "$exchanger + 2000000 lovelace + 1 $activityToken" \
    --tx-out-datum-embed-file $buyerExchangerDatum \
    --change-address $spenderAddress \
    --protocol-params-file $baseDir/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $activityMinterFile \
    --mint-redeemer-file $mintActivityTokenFile \
    --invalid-before $startSlot \
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
