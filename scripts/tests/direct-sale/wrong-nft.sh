set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$thisDir/generic-failure-case.sh "$baseDir/failure-cases/direct-sale/wrong-nft-swap-tx.sh"
