set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../
bn=$(basename $0)

$baseDir/wait/until-next-block.sh

# echo Mint
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-0-policy-n.sh 100
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 0

echo Mint Index NFT
$baseDir/compile.sh
$baseDir/update-start-time.sh 300000
$baseDir/happy-path/mint-index-nft-tx.sh
$baseDir/wait/until-next-block.sh

echo Start Auction
$baseDir/happy-path/lock-tx.sh 300000 0 true

startTime=$(date +%s)

sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 1
$baseDir/accounts/diff-accounts.sh $bn 0 1

echo Early Close Fails
detected=false

"$baseDir/failure-cases/close-too-early-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 2
$baseDir/accounts/diff-accounts.sh $bn 1 2

echo First Bid
$baseDir/happy-path/bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 3
$baseDir/accounts/diff-accounts.sh $bn 2 3

$baseDir/happy-path/collect-1-tx.sh


endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime))
sleepTime=$((440 - $elapsedTime))
finalTime=$(($sleepTime+$endTime))
currentTime=$endTime

while [ $currentTime -le $finalTime ]
do
  timeLeft=$(($finalTime - $currentTime))
  echo "$timeLeft left to sleep"
  $baseDir/wait/until-next-block.sh
  currentTime=$(date +%s)
done

echo Close with Wrong Payout Fails
detected=false

"$baseDir/failure-cases/close-wrong-payout-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 4
$baseDir/accounts/diff-accounts.sh $bn 3 4

echo Close
$baseDir/happy-path/close-bid-1-expired-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

$baseDir/accounts/log-all-accounts.sh $bn 5
$baseDir/accounts/diff-accounts.sh $bn 4 5

echo Bid on Expired Auction Fails
detected=false
"$baseDir/failure-cases/bid-on-expired-auction.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/accounts/log-all-accounts.sh $bn 6
$baseDir/accounts/diff-accounts.sh $bn 5 6

echo "Exchange tokens"
$baseDir/happy-path/exchange-buyer.sh
$baseDir/wait/until-next-block.sh
