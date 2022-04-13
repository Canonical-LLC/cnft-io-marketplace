set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp

DATUM_PREFIX=${1:-0}

$baseDir/core/direct-sale/emergency-close-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr) \
  ~/$BLOCKCHAIN_PREFIX/marketplace.skey \
  "1724100 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-emergency-close.json \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-emergency-close-hash.txt) \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
