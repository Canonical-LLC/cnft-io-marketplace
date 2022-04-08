set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

bodyFile=temp/lock-tx-body.01
outFile=temp/lock-tx.01

walletAddr=$1
signingKey=$2
value=$3
scriptDatumHash=$4
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/direct-sale.addr)

changeOutput=$(cardano-cli-balance-fixer change --address $walletAddr $BLOCKCHAIN -o "$value")

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $walletAddr $BLOCKCHAIN) \
    --tx-out "$scriptHash + $value" \
    --tx-out-datum-hash $scriptDatumHash \
    --tx-out "$walletAddr + 1744798 lovelace $extraOutput" \
    --change-address $walletAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $signingKey \
    $BLOCKCHAIN \
    --out-file $outFile

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile
