set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp

DATUM_PREFIX=${1:-0}

bodyFile=temp/swap-tx-body.01
outFile=temp/swap-tx.01
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
value1="1724100 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"
value2="1724100 lovelace + 1 380eab015ac8e52853df3ac291f0511b8a1b7d9ee77248917eaeef10.123456"
sellerAmount="8000000 lovelace"
marketPlaceAmount="1000000 lovelace"
datumFile1=$tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy.json
datumFile2=$tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy2.json
royalitiesAddr=$(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr)
royalitiesAmount="1000000 lovelace"
redeemerFile="$tempDir/$BLOCKCHAIN_PREFIX/redeemers/$DATUM_PREFIX/buy.json"
datumHash1=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-hash.txt)
datumHash2=$(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy2-hash.txt)
spenderAddress=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
nftValidatorFile=$baseDir/../../artifct-lbuc-sc/scripts/artifct-lbuc.plutus
scriptHash=$(cat scripts/$BLOCKCHAIN_PREFIX/direct-sale.addr)

utxoScript1=$(scripts/query/direct-sale.sh | grep $datumHash1 | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
utxoScript2=$(scripts/query/direct-sale.sh | grep $datumHash2 | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
changeOutput=$(cardano-cli-balance-fixer change --address $spenderAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $spenderAddress $BLOCKCHAIN ) \
    --tx-in $utxoScript1 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile1 \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in $utxoScript2 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile2 \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $spenderAddress $BLOCKCHAIN ) \
    --tx-out "$sellerAddr + $sellerAmount" \
    --tx-out "$buyerAddr + $value1" \
    --tx-out "$buyerAddr + $value2" \
    --tx-out "$marketplaceAddr + $marketPlaceAmount" \
    --tx-out "$royalitiesAddr + $royalitiesAmount" \
    --tx-out "$spenderAddress + 3000000 lovelace $extraOutput" \
    --change-address $spenderAddress \
    --protocol-params-file $baseDir/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile
