set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

nftValidatorFile=$baseDir/escrow.plutus
bidderAddress=$1
signingKey=$2
scriptDatumHash=$3
output=$4
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/escrow.addr)
scriptHash=$(cat $baseDir/escrow-hash.txt)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $bidderAddress $BLOCKCHAIN -o "$output")

currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+150))

bidMinterId=$(cat $baseDir/bid-minter-hash.txt)
bidMinterFile=$baseDir/bid-minter.plutus
bidMinterRedeemer=$baseDir/redeemers/mint.json

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

mintValue="1 $bidMinterId.$scriptHash"

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $bidderAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $bidderAddress $BLOCKCHAIN) \
    --tx-out "$scriptAddr + $output + $mintValue" \
    --tx-out-datum-hash $scriptDatumHash \
    --tx-out "$bidderAddress + 2000000 lovelace $extraOutput" \
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
