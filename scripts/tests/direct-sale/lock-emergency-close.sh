set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/lock-emergency-close-tx.sh 0 1
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/emergency-close-tx.sh
$baseDir/wait/until-next-block.sh
