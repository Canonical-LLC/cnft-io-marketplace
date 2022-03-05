{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Generic

import Canonical.Auction
import Canonical.Escrow
import Prelude

data Options = Options
  { batcherOutput     :: FilePath
  , batcherHashOutput :: FilePath
  , escrowOutput      :: FilePath
  , escrowHashOutput  :: FilePath
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
