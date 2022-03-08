set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/bid-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-reserve-not-met-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-reserve-not-met.json \
  "7000000 lovelace"
