set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/minting/mint-cnftio-for-marketplace.sh
$baseDir/wait/until-next-block.sh

$thisDir/start-bid1-bid2-close.sh
$thisDir/start-close-with-no-bids.sh
$thisDir/bid-cancel.sh
# $thisDir/two-script-bid-bid.sh
# $thisDir/two-script-txs-fail.sh

$thisDir/direct-sale/all.sh
