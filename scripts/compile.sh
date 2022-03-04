set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..

(
cd $mainDir
cabal run exe:create-auction-sc -- \
  --batcher-output=scripts/auction.plutus \
  --escrow-output=scripts/escrow.plutus
)

$thisDir/hash-plutus.sh
