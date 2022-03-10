set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
bn=$(basename $0)

$baseDir/update-start-time.sh

echo Bid
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Cancel
$baseDir/happy-path/bid-1-cancel-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1
