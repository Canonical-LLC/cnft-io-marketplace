set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..
$baseDir/hash-plutus.sh
bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01



exchangerAddr=$1
signingKey=$2

buyerAddr=$3
buyerValue=$4
buyerDatumHash=$5
buyerDatum=${6}

nftValue=${7}
oldNftDatum=${8}
newNftDatum=${9}
newOutput=${10}

validatorFile=$baseDir/exchanger.plutus
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/exchanger.addr)

buyerUtxo=$(scripts/query/exchanger.sh | grep $buyerDatumHash | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
indexNftUtxo=$(scripts/query/exchanger.sh | grep $nftValue | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)

changeOutput=$(cardano-cli-balance-fixer change --address $exchangerAddr $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

activityToken="$(cat $baseDir/activity-minter-hash.txt).4143544956495459"
mintValue="-1 $activityToken"
activityMinterFile=$baseDir/activity-minter.plutus
burnActivityTokenFile=$baseDir/redeemers/burn.json

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $exchangerAddr $BLOCKCHAIN ) \
    --tx-in $indexNftUtxo \
    --tx-in-script-file $validatorFile \
    --tx-in-datum-file $oldNftDatum \
    --tx-in-redeemer-value 42 \
    --tx-in $buyerUtxo \
    --tx-in-script-file $validatorFile \
    --tx-in-datum-file $buyerDatum \
    --tx-in-redeemer-value 42 \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $exchangerAddr $BLOCKCHAIN) \
    --tx-out "$buyerAddr + $buyerValue" \
    --tx-out "$scriptAddr + 2000000 lovelace + 1 $nftValue + $newOutput" \
    --tx-out-datum-embed-file $newNftDatum \
    --tx-out "$exchangerAddr + 3000000 lovelace $extraOutput" \
    --change-address $exchangerAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $activityMinterFile \
    --mint-redeemer-file $burnActivityTokenFile \
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
