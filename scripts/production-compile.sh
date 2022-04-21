set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..

utxo=${1:-$(./scripts/query/seller.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)}

(
cd $mainDir
cabal run exe:create-sc -- \
  --batcher-output=scripts/auction.plutus \
  --batcher-hash-output=scripts/auction-hash.txt \
  --escrow-output=scripts/escrow.plutus \
  --escrow-hash-output=scripts/escrow-hash.txt \
  --bid-minter-output=scripts/bid-minter.plutus \
  --bid-minter-hash-output=scripts/bid-minter-hash.txt \
  --exchanger-output=scripts/exchanger.plutus \
  --exchanger-hash-output=scripts/exchanger-hash.txt \
  --activity-minter-output=scripts/activity-minter.plutus \
  --activity-minter-hash-output=scripts/activity-minter-hash.txt \
  --global-nft-minter-output=scripts/global-nft-minter.plutus \
  --global-nft-minter-hash-output=scripts/global-nft-minter-hash.txt \
  --global-nft-utxo=$utxo \
  --rate-numerator=-0 \
  --rate-denominator=1 \
  --initial-amount=1 \
  --token-name="CNFT" \
  --policy-id=6cfbfedd8c8ea23d264f5ae3ef039217100c210bb66de8711f21c903 \
  --direct-sale-output=scripts/direct-sale.plutus \
  --direct-sale-hash-output=scripts/direct-sale-hash.txt \
  --exchanger-emergency-pkh=01b05e7d27917ead7f23f99ae177091ef86b739538793e3677f1273c
)


$thisDir/hash-plutus.sh
