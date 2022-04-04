set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh


exchangedToken="$(cat $baseDir/test-policies/test-policy-0-id.txt).123456"

minterAddress=$1
signingKey=$2
scriptDatumFile=$3
nftPolicyId=$(cat $baseDir/global-nft-minter-hash.txt)
mintValue="1 $nftPolicyId.494E444558"
output="2000000 lovelace + $mintValue + 100 $exchangedToken"
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/exchanger.addr)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $minterAddress $BLOCKCHAIN -o "$output")

indexMinterFile=$baseDir/global-nft-minter.plutus
indexMinterRedeemer=$baseDir/redeemers/mint.json

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi


cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $minterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $minterAddress $BLOCKCHAIN) \
    --tx-out "$scriptAddr + $output" \
    --tx-out-datum-embed-file $scriptDatumFile \
    --tx-out "$minterAddress + 2689596 lovelace $extraOutput" \
    --change-address $minterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $indexMinterFile \
    --mint-redeemer-file $indexMinterRedeemer \
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
