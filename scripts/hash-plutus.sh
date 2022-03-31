set -eux
thisDir=$(dirname "$0")

cardano-cli address build \
  --payment-script-file $thisDir/auction.plutus \
  $BLOCKCHAIN \
  --out-file $thisDir/$BLOCKCHAIN_PREFIX/auction.addr

cardano-cli address build \
  --payment-script-file $thisDir/escrow.plutus \
  $BLOCKCHAIN \
  --out-file $thisDir/$BLOCKCHAIN_PREFIX/escrow.addr

cardano-cli address build \
  --payment-script-file $thisDir/exchanger.plutus \
  $BLOCKCHAIN \
  --out-file $thisDir/$BLOCKCHAIN_PREFIX/exchanger.addr
