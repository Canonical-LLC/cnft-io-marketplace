{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Generic
import           Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Canonical.Auction
import Canonical.Escrow
import Canonical.BidMinter
import Canonical.ActivityMinter
import Canonical.ActivityTokenExchanger
import Prelude
import Data.String

data Options = Options
  { batcherOutput             :: FilePath
  , batcherHashOutput         :: FilePath
  , escrowOutput              :: FilePath
  , escrowHashOutput          :: FilePath
  , bidMinterOutput           :: FilePath
  , bidMinterHashOutput       :: FilePath
  , exchangerOutput           :: FilePath
  , exchangerHashOutput       :: FilePath
  , activityMinterOutput      :: FilePath
  , activityMinterHashOutput  :: FilePath
  , globalNftMinterOutput     :: FilePath
  , globalNftMinterHashOutput :: FilePath
  , globalNftUtxo             :: String
  , rateNumerator             :: Int
  , rateDenominator           :: Int
  , initialAmount             :: Int
  , tokenName                 :: String
  , policyId                  :: String
  } deriving (Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = run =<< getRecord "Auction compiler"

globalNftTokenName :: TokenName
globalNftTokenName = "INDEX"

parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y

run :: Options -> IO ()
run Options{..} = do

  let theUtxo = parseUTxO globalNftUtxo

  writeFileTextEnvelope globalNftMinterOutput Nothing (globalNft globalNftTokenName theUtxo) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ globalNftMinterOutput

  let globalNftPolicy = globalNftPolicyId globalNftTokenName theUtxo

  writeFile globalNftMinterHashOutput $ show globalNftPolicy

  let exchangerConfig = ExchangerConfig
        { ecRateNumerator    = fromIntegral rateNumerator
        , ecRateDenominator  = fromIntegral rateDenominator
        , ecInitialAmount    = fromIntegral initialAmount
        , ecTokenName        = fromString tokenName
        , ecPolicyId         = fromString policyId
        , ecGlobalCounterNft = globalNftPolicy
        }

  writeFileTextEnvelope escrowOutput Nothing escrowScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ escrowOutput

  writeFile escrowHashOutput $ show escrowValidatorHash

  writeFileTextEnvelope bidMinterOutput Nothing bid >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote minter to file " ++ bidMinterOutput

  writeFile bidMinterHashOutput $ show bidPolicyId

  writeFileTextEnvelope exchangerOutput Nothing (exchanger exchangerConfig) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote exchanger to file " ++ exchangerOutput

  let theExchangerHash = exchangerHash exchangerConfig

  writeFile exchangerHashOutput $ show theExchangerHash

  writeFileTextEnvelope batcherOutput Nothing (auctionScript theExchangerHash) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ batcherOutput

  let theAuctionHash = auctionScriptHash theExchangerHash

  writeFile batcherHashOutput $ show theAuctionHash

  writeFileTextEnvelope activityMinterOutput Nothing (activity [theAuctionHash]) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote activity minter to file " ++ activityMinterOutput

  writeFile activityMinterHashOutput $ show $ activityPolicyId [theAuctionHash]
