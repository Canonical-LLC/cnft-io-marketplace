set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

offset=${1:-500000}
DATUM_PREFIX=${2:-0}

if [ -z "${3+xxx}" ]; then
$baseDir/update-start-time.sh $offset $DATUM_PREFIX
fi


sleep 2

$baseDir/core/lock-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start-hash.txt) \
  "1758582 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"
