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
import           Ledger.Typed.Scripts
import           PlutusTx
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Credential
import           Canonical.Escrow
import           Canonical.Shared
import qualified PlutusTx.AssocMap as M
#include "DebugUtilities.h"

-------------------------------------------------------------------------------
-- Custom ScriptContext types to improvement transaction size and memory usage
-------------------------------------------------------------------------------
data BidAddress = BidAddress
  { baddressCredential        :: Credential
  , baddressStakingCredential :: BuiltinData
  }

data BidTxOut = BidTxOut
  { btxOutAddress             :: BidAddress
  , btxOutValue               :: Value
  , btxOutDatumHash           :: Maybe DatumHash
  }

data BidTxInfo = BidTxInfo
  { btxInfoInputs             :: BuiltinData
  , btxInfoOutputs            :: [BidTxOut]
  , btxInfoFee                :: BuiltinData
  , btxInfoMint               :: Value
  , btxInfoDCert              :: BuiltinData
  , btxInfoWdrl               :: BuiltinData
  , btxInfoValidRange         :: POSIXTimeRange
  , btxInfoSignatories        :: BuiltinData
  , btxInfoData               :: [(DatumHash, Datum)]
  , btxInfoId                 :: BuiltinData
  }

data BidScriptPurpose
    = BMinting CurrencySymbol

data BidScriptContext = BidScriptContext
  { bScriptContextTxInfo  :: BidTxInfo
  , bScriptContextPurpose :: BidScriptPurpose
  }

convertOutput
  :: UnsafeFromData a
  => [(DatumHash, Datum)]
  -> BidTxOut
  -> (a, Value)
convertOutput datums BidTxOut {btxOutDatumHash = Just dh, btxOutValue} = case find ((==dh) . fst) datums of
  Just (_, Datum bs) -> (unsafeFromBuiltinData bs, btxOutValue)
  Nothing -> TRACE_ERROR("Could not find the datum")
convertOutput _ _ = TRACE_ERROR("Output is missing a datum hash")

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data Action = A_Mint | A_Burn

data BidData = BidData
  { bdBid            :: Integer
  , bdValue          :: Value
  , bdValidStartTime :: POSIXTime
  , bdValidEndTime   :: POSIXTime
  , bdExpiration     :: POSIXTime
  }

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

unstableMakeIsData ''Action
unstableMakeIsData ''BidData

unstableMakeIsData ''BidAddress
unstableMakeIsData ''BidTxOut
unstableMakeIsData ''BidTxInfo
unstableMakeIsData ''BidScriptPurpose
unstableMakeIsData ''BidScriptContext

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------


hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value v) c t = case M.lookup c v of
  Nothing -> False
  Just m -> case M.toList m of
    [(tn, ct)] -> tn == t && ct == 1
    _ -> False

mkPolicy :: Action -> BidScriptContext -> Bool
mkPolicy action BidScriptContext
    { bScriptContextTxInfo  = BidTxInfo {..}
    , bScriptContextPurpose = BMinting theCurrencySymbol
    } = case action of
  A_Burn -> case btxInfoMint of
    Value v -> case M.lookup theCurrencySymbol v of
      Nothing -> TRACE_ERROR("No tokens to burn")
      Just m -> case M.toList m of
        [(_, c)] -> c < 0
        _ -> TRACE_ERROR("Must burn this policy")
  A_Mint ->

    let

      onlyOutput :: BidTxOut
      onlyOutput = case filter (\x -> M.member theCurrencySymbol (getValue (btxOutValue x))) btxInfoOutputs of
        [o] -> o
        _ -> TRACE_ERROR("Expected exactly one output")

      outputAddress :: BidAddress
      outputAddress = btxOutAddress onlyOutput

      outputTokenName :: TokenName
      outputTokenName = TokenName $ case outputAddress of
        BidAddress { baddressCredential } -> case baddressCredential of
          PubKeyCredential (PubKeyHash    bs) -> bs
          ScriptCredential (ValidatorHash bs) -> bs

      onlyOneTokenMinted :: Bool
      onlyOneTokenMinted =
        hasSingleToken
          btxInfoMint
          theCurrencySymbol
          outputTokenName

      correctTokenMinted = traceIfFalse "Wrong mint amount!" onlyOneTokenMinted


      escrowValue :: Value
      (EscrowLockerInput { eliData = BidData { bdBid, bdValidStartTime, bdValidEndTime }}, escrowValue) =
        convertOutput btxInfoData onlyOutput

      bidAmountCorrect =
        TRACE_IF_FALSE("Output bid amount mismatch",
          (bdBid <= lovelaces escrowValue))

      bidValidRangeCorrect =
        TRACE_IF_FALSE("Output bid validity range mismatch",
          ( interval bdValidStartTime bdValidEndTime `contains` btxInfoValidRange))

    in correctTokenMinted
    && bidAmountCorrect
    && bidValidRangeCorrect

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
wrappedPolicy :: WrappedMintingPolicyType
wrappedPolicy = wrapMint mkPolicy

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
