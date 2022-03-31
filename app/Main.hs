{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Generic

import Canonical.Auction
import Canonical.Escrow
import Canonical.BidMinter
import Canonical.ActivityMinter
import Canonical.ActivityTokenExchanger
import Prelude

data Options = Options
  { batcherOutput            :: FilePath
  , batcherHashOutput        :: FilePath
  , escrowOutput             :: FilePath
  , escrowHashOutput         :: FilePath
  , bidMinterOutput          :: FilePath
  , bidMinterHashOutput      :: FilePath
  , exchangerOutput          :: FilePath
  , exchangerHashOutput      :: FilePath
  , activityMinterOutput     :: FilePath
  , activityMinterHashOutput :: FilePath
  } deriving (Show, Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = run =<< getRecord "Auction compiler"

run :: Options -> IO ()
run Options{..} = do

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

  writeFileTextEnvelope exchangerOutput Nothing exchanger >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote exchanger to file " ++ exchangerOutput

  writeFile exchangerHashOutput $ show exchangerHash

  writeFileTextEnvelope activityMinterOutput Nothing (activity [auctionScriptHash]) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote activity minter to file " ++ activityMinterOutput

  writeFile activityMinterHashOutput $ show $ activityPolicyId [auctionScriptHash]
