set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp

DATUM_PREFIX=${1:-0}

$baseDir/generate-datums.sh 100000

$baseDir/core/direct-sale/lock-1-input-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  "1724100 lovelace" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/buy-hash.txt)
