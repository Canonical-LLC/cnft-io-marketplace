set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../
bn=$(basename $0)

$baseDir/wait/until-next-block.sh

# echo Mint
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Start Auction
$baseDir/happy-path/lock-tx.sh 400000 0

startTime=$(date +%s)

sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1

$baseDir/failure-cases/reserve-not-met-tx.sh

$baseDir/accounts/log-all-accounts.sh $bn 2
$baseDir/accounts/diff-accounts.sh $bn 1 2

$baseDir/wait/until-next-block.sh

$baseDir/failure-cases/reserve-not-met-collect-tx.sh
