set -eu

thisDir=$(dirname "$0")
baseDir=$thisDir/../..

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-cnftio-for-marketplace.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/direct-sale/lock-tx.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/failure-cases/direct-sale/close-no-timeout.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "failed!"
  $baseDir/wait/until-next-block.sh
  $baseDir/scratch/buyer-to-seller-utxo.sh
  $baseDir/wait/until-next-block.sh
  exit 1
fi

echo "Success!"
