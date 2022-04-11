set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$thisDir/generic-failure-case.sh "$baseDir/failure-cases/seller-has-no-activity-token.sh"
