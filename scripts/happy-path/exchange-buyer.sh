set -eu
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

nftPolicyId=$(cat $baseDir/global-nft-minter-hash.txt)
nftAsset=$nftPolicyId.494e444558
exchangedToken="$(cat $baseDir/test-policies/test-policy-0-id.txt).123456"

$baseDir/core/exchange-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr) \
  ~/$BLOCKCHAIN_PREFIX/marketplace.skey \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  "1700000 lovelace + 10 $exchangedToken" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/sellerExchange-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/sellerExchange.json \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  "1700000 lovelace + 10 $exchangedToken" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buyerExchange-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buyerExchange.json \
  $nftAsset \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/index0.json \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/index1.json
