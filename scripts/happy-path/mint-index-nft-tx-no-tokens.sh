set -eu
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

DATUM_PREFIX=${DATUM_PREFIX:-0}

nftPolicyId=$(cat $baseDir/global-nft-minter-hash.txt)
nftAsset=$nftPolicyId.494e444558

$baseDir/core/mint-index-nft-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/index0.json \
  "1689618 lovelace + 1 $nftAsset"
