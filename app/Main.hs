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

  writeFileTextEnvelope escrowOutput Nothing (globalNft globalNftTokenName theUtxo) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ escrowOutput

  let globalNftPolicy = globalNftPolicyId globalNftTokenName theUtxo

  writeFile escrowHashOutput $ show globalNftPolicy

  let exchangerConfig = ExchangerConfig
        { ecRateNumerator    = -1
        , ecRateDenominator  = 2
        , ecInitialAmount    = 100
        , ecTokenName        = "TOKEN"
        , ecPolicyId         = "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
        , ecGlobalCounterNft = globalNftPolicy
        }

  writeFileTextEnvelope escrowOutput Nothing escrowScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ escrowOutput

  writeFile escrowHashOutput $ show escrowValidatorHash

  writeFileTextEnvelope batcherOutput Nothing auctionScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ batcherOutput

  writeFile batcherHashOutput $ show auctionScriptHash

  writeFileTextEnvelope bidMinterOutput Nothing bid >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote minter to file " ++ bidMinterOutput

  writeFile bidMinterHashOutput $ show bidPolicyId

  writeFileTextEnvelope exchangerOutput Nothing (exchanger exchangerConfig) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote exchanger to file " ++ exchangerOutput

  writeFile exchangerHashOutput $ show $ exchangerHash exchangerConfig

  writeFileTextEnvelope activityMinterOutput Nothing (activity [auctionScriptHash]) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote activity minter to file " ++ activityMinterOutput

  writeFile activityMinterHashOutput $ show $ activityPolicyId [auctionScriptHash]
