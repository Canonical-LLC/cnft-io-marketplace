set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/bid-cancel-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-1-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-1.json
