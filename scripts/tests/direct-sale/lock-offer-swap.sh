set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/lock-offer-tx.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/swap-offer-tx.sh
$baseDir/wait/until-next-block.sh
$baseDir/scratch/buyer-to-seller-utxo.sh
$baseDir/wait/until-next-block.sh
