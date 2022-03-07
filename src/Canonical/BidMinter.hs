{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Canonical.BidMinter
  ( BidData (..)
  , bidPolicyId
  , bid
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger
import           Ledger.Value
import           Ledger.Ada
import           Ledger.Typed.Scripts
import           PlutusTx
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Credential
import           Canonical.Escrow

#define DEBUG

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#endif

data Action = A_Mint | A_Burn

PlutusTx.unstableMakeIsData ''Action

data BidData = BidData
  { bdValue         :: Value
  , bdBid           :: Integer
  , bdBidValidRange :: POSIXTimeRange
  , bdExpiration    :: POSIXTime
  }

PlutusTx.unstableMakeIsData ''BidData

{-# INLINABLE convertOutput #-}
convertOutput
  :: UnsafeFromData a
  => [(DatumHash, Datum)]
  -> TxOut
  -> (a, Value)
convertOutput datums TxOut {txOutDatumHash = Just dh, txOutValue} = case find ((==dh) . fst) datums of
  Just (_, Datum bs) -> (unsafeFromBuiltinData bs, txOutValue)
  Nothing -> TRACE_ERROR("Could not find the datum")
convertOutput _ _ = TRACE_ERROR("Output is missing a datum hash")

lovelaces :: Value -> Integer
lovelaces = getLovelace . fromValue

hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken v c t = valueOf v c t == 1

mkPolicy :: Action -> ScriptContext -> Bool
mkPolicy action ctx@ScriptContext { scriptContextTxInfo = TxInfo {..} } = case action of
  A_Burn ->
    let
      theCurrencySymbol :: CurrencySymbol
      theCurrencySymbol = ownCurrencySymbol ctx

    in  case flattenValue txInfoMint of
          [(sym, _, c)]
            -> sym == theCurrencySymbol
            && c < 0
          _ -> False
  A_Mint ->
    let
      theCurrencySymbol :: CurrencySymbol
      theCurrencySymbol = ownCurrencySymbol ctx

      onlyOutput :: TxOut
      onlyOutput = case filter (\x -> hasSingleToken (txOutValue x) theCurrencySymbol
          outputTokenName) txInfoOutputs of
        [o] -> o
        _ -> traceError "Expected exactly one output"

      outputAddress :: Address
      outputAddress = txOutAddress onlyOutput

      outputTokenName :: TokenName
      outputTokenName = TokenName $ case outputAddress of
        Address { addressCredential } -> case addressCredential of
          PubKeyCredential (PubKeyHash    bs) -> bs
          ScriptCredential (ValidatorHash bs) -> bs

      onlyOneTokenMinted :: Bool
      onlyOneTokenMinted =
        hasSingleToken
          txInfoMint
          theCurrencySymbol
          outputTokenName

      correctTokenMinted = traceIfFalse "Wrong mint amount!" onlyOneTokenMinted

      escrowValue :: Value

      (EscrowLockerInput { eliData = BidData { bdBid, bdBidValidRange }}, escrowValue) =
        convertOutput txInfoData onlyOutput

      bidAmountCorrect =
        traceIfFalse "Output bid amount mismatch"
          (bdBid < lovelaces escrowValue)

      bidValidRangeCorrect =
        traceIfFalse "Output bid validity range mismatch"
          (txInfoValidRange == bdBidValidRange)

    in correctTokenMinted
    && bidAmountCorrect
    && bidValidRangeCorrect

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
wrappedPolicy :: WrappedMintingPolicyType
wrappedPolicy = wrapMintingPolicy mkPolicy

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(compile [|| wrappedPolicy ||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript

bidPolicyId :: CurrencySymbol
bidPolicyId = mpsSymbol $ mintingPolicyHash policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

bid :: PlutusScript PlutusScriptV1
bid
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  $ scriptAsCbor
