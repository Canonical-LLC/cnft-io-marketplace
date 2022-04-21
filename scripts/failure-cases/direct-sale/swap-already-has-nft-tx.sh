set -eux

DATUM_PREFIX=${1:-0}

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp

$baseDir/core/direct-sale/swap-buy-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  "2172366 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  "8000000 lovelace" \
  "1000000 lovelace" \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy.json \
  $(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr) \
  "1000000 lovelace" \
  "$tempDir/$BLOCKCHAIN_PREFIX/redeemers/$DATUM_PREFIX/buy.json" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-hash.txt) \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr) \
  ~/$BLOCKCHAIN_PREFIX/marketplace.skey
