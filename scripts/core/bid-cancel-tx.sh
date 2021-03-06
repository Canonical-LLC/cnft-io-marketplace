set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

escrowFile=$baseDir/escrow.plutus
redeemerFile=$baseDir/redeemers/cancel.json
bidderAddress=$1
signingKey=$2
scriptDatumHash=$3
scriptDatumFile=$4
scriptHash=$(cat $baseDir/escrow-hash.txt)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01

bidMinterId=$(cat $baseDir/bid-minter-hash.txt)
bidMinterFile=$baseDir/bid-minter.plutus
bidMinterRedeemer=$baseDir/redeemers/burn.json
mintValue="-1 $bidMinterId.$scriptHash"

changeOutput=$(cardano-cli-balance-fixer change --address $bidderAddress $BLOCKCHAIN -o "$mintValue")

escrowScriptUtxo=$(scripts/query/escrow.sh | grep $scriptHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)

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
    $(cardano-cli-balance-fixer input --address $bidderAddress $BLOCKCHAIN) \
    --tx-in $escrowScriptUtxo \
    --tx-in-script-file $escrowFile \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in-datum-file $scriptDatumFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $bidderAddress $BLOCKCHAIN) \
    --tx-out "$bidderAddress + 2137884 lovelace $extraOutput" \
    --change-address $bidderAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $bidMinterFile \
    --mint-redeemer-file $bidMinterRedeemer \
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
