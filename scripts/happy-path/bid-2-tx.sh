set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/bid-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-2-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-2.json \
  "30000000 lovelace"
