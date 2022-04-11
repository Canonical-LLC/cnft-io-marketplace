set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$thisDir/generic-failure-case.sh "$baseDir/failure-cases/direct-sale/too-little-ada-royalties.sh"
