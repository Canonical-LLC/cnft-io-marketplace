{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Generic
import           Ledger
import Ledger.Bytes                        (getLedgerBytes)
import Canonical.Auction
import Canonical.Escrow
import Canonical.BidMinter
import Canonical.DirectSale
import Prelude
import Data.String

data Options = Options
  { batcherOutput             :: FilePath
  , batcherHashOutput         :: FilePath
  , escrowOutput              :: FilePath
  , escrowHashOutput          :: FilePath
  , bidMinterOutput           :: FilePath
  , bidMinterHashOutput       :: FilePath
  , tokenName                 :: String
  , policyId                  :: String
  , directSaleOutput          :: String
  , directSaleHashOutput      :: String
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

  writeFileTextEnvelope escrowOutput Nothing escrowScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ escrowOutput

  writeFile escrowHashOutput $ show escrowValidatorHash

  writeFileTextEnvelope bidMinterOutput Nothing bid >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote minter to file " ++ bidMinterOutput

  writeFile bidMinterHashOutput $ show bidPolicyId

  writeFileTextEnvelope batcherOutput Nothing auctionScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ batcherOutput

  let theAuctionHash = auctionScriptHash

  writeFile batcherHashOutput $ show theAuctionHash

  writeFileTextEnvelope directSaleOutput Nothing directSale >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ directSaleOutput

  let theDirectSaleHash = directSaleHash

  writeFile directSaleHashOutput $ show theDirectSaleHash
