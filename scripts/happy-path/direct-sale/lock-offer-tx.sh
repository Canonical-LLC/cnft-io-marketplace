set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../..
tempDir=$baseDir/../temp

DATUM_PREFIX=${1:-0}
offset=${2:-500000}

$baseDir/generate-datums.sh $offset
sleep 2
$baseDir/hash-datums.sh

$baseDir/core/direct-sale/lock-1-input-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  "10000000 lovelace" \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/offer-hash.txt)
