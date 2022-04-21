set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-cnftio-for-marketplace.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/lock-tx.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/cancel-tx.sh
$baseDir/wait/until-next-block.sh
