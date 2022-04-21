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
  --token-name="CNFTIO" \
  --policy-id=$(cat $thisDir/test-policies/test-policy-0-id.txt) \
  --direct-sale-output=scripts/direct-sale.plutus \
  --direct-sale-hash-output=scripts/direct-sale-hash.txt
)

$thisDir/hash-plutus.sh
