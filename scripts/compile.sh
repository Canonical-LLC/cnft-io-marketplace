set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..

(
cd $mainDir
cabal run exe:create-auction-sc -- \
  --batcher-output=scripts/auction.plutus \
  --batcher-hash-output=scripts/auction-hash.txt \
  --escrow-output=scripts/escrow.plutus \
  --escrow-hash-output=scripts/escrow-hash.txt \
  --bid-minter-output=scripts/bid-minter.plutus \
  --bid-minter-hash-output=scripts/bid-minter-hash.txt
)

$thisDir/hash-plutus.sh
