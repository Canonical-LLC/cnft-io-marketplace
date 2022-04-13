set -eux

thisDir=$(dirname "$0")

$thisDir/lock-cancel.sh
$thisDir/lock-swap.sh
$thisDir/lock-offer-swap.sh
$thisDir/lock-offer-close.sh
$thisDir/nft-goes-to-wrong-address.sh
$thisDir/too-little-ada.sh
$thisDir/too-little-fees.sh
$thisDir/too-little-royalties.sh
$thisDir/wrong-nft.sh
$thisDir/missing-nft.sh
$thisDir/double-swap-with-same-datum-hash.sh
$thisDir/buyer-has-no-activity-token.sh
$thisDir/seller-has-no-activity-token.sh
$thisDir/close-assets-not-returned.sh
$thisDir/close-too-early.sh
$thisDir/close-with-no-timeout.sh
$thisDir/lock-emergency-close-wrong-closer.sh
$thisDir/lock-emergency-close.sh
