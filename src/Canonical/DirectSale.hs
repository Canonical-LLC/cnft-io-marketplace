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
  ( PubKeyHash
  , Address (..)
  , ValidatorHash
  )
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Contexts
import           Ledger.Typed.Scripts
import           Plutus.V1.Ledger.Credential

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Payout = Payout
  { pAddress :: PubKeyHash
  , pValue   :: Value
  }

data SwapInput = SwapInput
  { siOwner         :: PubKeyHash
  -- ^ Used for the signer check on Cancel
  , siSwapPayouts   :: [Payout]
  -- ^ Divvy up the payout to different address for Swap
  }

data BuyerInput = Cancel | Buy [Payout]

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
instance Eq Payout where
  x == y
    =  pAddress x == pAddress y
    && pValue x == pValue y

instance Eq SwapInput where
  x == y
    =  siOwner x == siOwner y
    && siSwapPayouts x == siSwapPayouts y

instance Eq BuyerInput where
  x == y = case (x, y) of
    (Cancel, Cancel) -> True
    (Buy a, Buy b) -> a == b
    _ -> False

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
        -> traceIfFalse "Not signed by NFT seller!"
        $  info `txSignedBy` siOwner l

    Buy unlockerPayouts ->
      let
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

      in traceIfFalse "Outputs are invalid!" outputsAreValid
      && traceIfFalse "Input value is incorrect" scriptInputIsValid

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
data Buying
instance ValidatorTypes Buying where
  type instance DatumType Buying = SwapInput
  type instance RedeemerType Buying = BuyerInput

validator :: TypedValidator Buying
validator =
  mkTypedValidator @Buying
    $$(PlutusTx.compile [|| buyOfferValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator

directSaleHash :: ValidatorHash
directSaleHash = validatorHash validator

directSale :: PlutusScript PlutusScriptV1
directSale
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ validatorScript validator
