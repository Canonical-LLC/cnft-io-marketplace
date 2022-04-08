module Canonical.DirectSale
    ( Payout (..)
    , SwapInput (..)
    , BuyerInput  (..)
    , directSale
    , directSaleHash
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Credential
import           Canonical.Shared
import qualified Plutus.V1.Ledger.Scripts as Scripts
#include "DebugUtilities.h"
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Payout = Payout
  { pAddress :: PubKeyHash
  , pValue   :: Value
  }

data CloseInfo = CloseInfo
  { ciTimeout :: POSIXTime
  , ciValue   :: Value
  }

data SwapInput = SwapInput
  { siOwner         :: PubKeyHash
  -- ^ Used for the signer check on Cancel
  , siSwapPayouts   :: [Payout]
  -- ^ Divvy up the payout to different address for Swap
  , siCloseInfo     :: Maybe CloseInfo
  }

data BuyerInput = Cancel | Buy [Payout] | Close

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"

  in txOutValue . txInInfoResolved $ input

payoutToInequality :: Payout -> (PubKeyHash, Value)
payoutToInequality Payout {..} = (pAddress, pValue)

mergePayoutsValue :: [Payout] -> Value
mergePayoutsValue = foldr (\x acc -> pValue x <> acc) mempty

paidAtleastTo :: TxInfo -> PubKeyHash -> Value -> Bool
paidAtleastTo info pkh val = valuePaidTo info pkh `geq` val

mergeInequalities
  :: Map PubKeyHash Value
  -> Map PubKeyHash Value
  -> Map PubKeyHash Value
mergeInequalities = M.unionWith (+)

mergeAll :: [Map PubKeyHash Value] -> Map PubKeyHash Value
mergeAll = foldr mergeInequalities M.empty

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq CloseInfo where
  x == y
    =  ciTimeout x == ciTimeout y
    && ciValue   x == ciValue   y

instance Eq Payout where
  x == y
    =  pAddress x == pAddress y
    && pValue x == pValue y

instance Eq SwapInput where
  x == y
    =  siOwner       x == siOwner       y
    && siSwapPayouts x == siSwapPayouts y
    && siCloseInfo   x == siCloseInfo   y

instance Eq BuyerInput where
  x == y = case (x, y) of
    (Cancel, Cancel) -> True
    (Buy a, Buy b) -> a == b
    _ -> False

PlutusTx.unstableMakeIsData ''CloseInfo
PlutusTx.unstableMakeIsData ''Payout
PlutusTx.unstableMakeIsData ''SwapInput
PlutusTx.unstableMakeIsData ''BuyerInput


-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------
-- check that each user is paid
-- and the total is correct
validateOutputConstraints :: TxInfo -> Map PubKeyHash Value -> Bool
validateOutputConstraints info constraints
  = all (\(x, y) -> paidAtleastTo info x y)
  $ M.toList constraints

buyOfferValidator :: SwapInput -> BuyerInput -> ScriptContext -> Bool
buyOfferValidator l u ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo ctx

  -- Every branch but user initiated cancel requires checking the input
  -- to ensure there is only one script input.
  in case u of
    Cancel
        -> TRACE_IF_FALSE("Not signed by NFT seller!", (info `txSignedBy` siOwner l))

    Buy unlockerPayouts ->
      let
        isActive :: Bool
        isActive = case siCloseInfo l of
          Nothing -> True
          Just CloseInfo {..} -> ciTimeout `after` txInfoValidRange info

        scriptInput :: Value
        scriptInput = getOnlyScriptInput info

        scriptInputIsValid :: Bool
        scriptInputIsValid = scriptInput `geq` mergePayoutsValue unlockerPayouts

        outputsAreValid :: Bool
        outputsAreValid
          = validateOutputConstraints info
          $ mergeAll
          $ map ( uncurry M.singleton
                . payoutToInequality
                )
          $ siSwapPayouts l <> unlockerPayouts

      in TRACE_IF_FALSE("Outputs are invalid!", outputsAreValid)
      && TRACE_IF_FALSE("Input value is incorrect", scriptInputIsValid)
      && TRACE_IF_FALSE("Expired", isActive)

    Close -> case siCloseInfo l of
      Nothing -> TRACE_ERROR("Not expired")
      Just CloseInfo {..} ->
        let
          assetsReturnedToOwner :: Bool
          assetsReturnedToOwner = paidAtleastTo info (siOwner l) ciValue

          isExpired :: Bool
          isExpired = ciTimeout `before` txInfoValidRange info

          scriptInput :: Value
          scriptInput = getOnlyScriptInput info

          scriptInputIsValid :: Bool
          scriptInputIsValid = scriptInput `geq` ciValue

        in TRACE_IF_FALSE("Assets not returned to owner", assetsReturnedToOwner)
        && TRACE_IF_FALSE("Not expired", isExpired)
        && TRACE_IF_FALSE("Input value is incorrect", scriptInputIsValid)

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
wrapDirectSaleValidator
    :: BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapDirectSaleValidator = wrap buyOfferValidator

validator :: Scripts.Validator
validator = Scripts.mkValidatorScript $
  $$(PlutusTx.compile [|| wrapDirectSaleValidator ||])

directSaleHash :: ValidatorHash
directSaleHash = validatorHash validator

directSale :: PlutusScript PlutusScriptV1
directSale
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise validator
