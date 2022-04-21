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
import qualified Ledger.Ada as Ada
#include "DebugUtilities.h"
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Payout = Payout
  { pAddress :: PubKeyHash
  , pValue   :: Value
  }

data CloseInfo = CloseInfo
  { ciTimeout         :: Maybe POSIXTime
  -- ^ An optional timeout for expiration as an absolute
  --   time in milliseconds
  , ciEmergencyCloser :: Maybe PubKeyHash
  -- ^ The emergency closer public key hash
  , ciValue           :: Value
  -- ^ The value listed that must be returned if closed
  }

data SwapInput = SwapInput
  { siOwner             :: PubKeyHash
  -- ^ Used for the signer check on Cancel
  , siSwapPayouts       :: [Payout]
  -- ^ Divvy up the payout to different address for Swap
  , siCloseInfo         :: Maybe CloseInfo
  -- ^ Optional data for closing the listing on the behalf
  --   of the owner
  , siBoostTokenName    :: TokenName
  -- ^ The Boost token name
  , siBoostPolicyId     :: CurrencySymbol
  -- ^ The Boost policy id
  , siBoostPayoutPkh    :: PubKeyHash
  -- ^ The Boost payout public key hash
  , siBoostPayinPkh     :: PubKeyHash
  -- ^ The pkh that signs the tx to deliver activity boost tokens
  }

data BuyerInput = Cancel | Buy [Payout] | Close | EmergencyClose
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
    =  siOwner               x == siOwner             y
    && siSwapPayouts         x == siSwapPayouts       y
    && siCloseInfo           x == siCloseInfo         y
    && siBoostTokenName      x == siBoostTokenName    y
    && siBoostPolicyId       x == siBoostPolicyId     y
    && siBoostPayoutPkh      x == siBoostPayoutPkh    y

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
getContinuingOutputs'
  :: DataConstraint(a)
  => [(DatumHash, Datum)]
  -> ValidatorHash
  -> [TxOut]
  -> [(a, Value)]
getContinuingOutputs' datums vh outs =
  map
    (\TxOut {..} -> case txOutDatumHash of
          Just dh -> (extractData datums dh, txOutValue)
          Nothing -> TRACE_ERROR("Missing Datum Hash")
    )
    (filter
      (\TxOut {..} -> addressCredential txOutAddress
        == ScriptCredential vh)
      outs
    )

lovelaces :: Value -> Integer
lovelaces v = valueOf v Ada.adaSymbol Ada.adaToken

-- check that each user is paid
-- and the total is correct
validateOutputConstraints :: TxInfo -> Map PubKeyHash Value -> Bool
validateOutputConstraints info constraints
  = all (\(x, y) -> paidAtleastTo info x y)
  $ M.toList constraints

buyOfferValidator :: SwapInput -> BuyerInput -> ScriptContext -> Bool
buyOfferValidator l u ctx =
  let
    info@TxInfo{..} = scriptContextTxInfo ctx

  -- Every branch but user initiated cancel requires checking the input
  -- to ensure there is only one script input.
  in case u of
    Cancel
        -> TRACE_IF_FALSE("Not signed by NFT seller!", (info `txSignedBy` siOwner l))

    Buy unlockerPayouts ->
      let
        boostOf :: Value -> Integer
        boostOf v = valueOf v (siBoostPolicyId l) (siBoostTokenName l)

        buyerPkh :: PubKeyHash
        buyerPkh = case txInfoSignatories of
          xs@[_, _] -> case filter (/=siBoostPayinPkh l) xs of
            [x] -> x
            _ -> TRACE_ERROR("Expected one signer after filtering out the boost payin")
          _ -> TRACE_ERROR("Expected two signers")

        activityTokenAmount :: Integer
        activityTokenAmount
          = max 1
          $ foldr (\Payout {..} acc -> acc + lovelaces pValue) 0 (siSwapPayouts l)
          `divide` 20_000_000

        sellerGetsActivityToken :: Bool
        sellerGetsActivityToken
          = boostOf (valuePaidTo info (siOwner l)) >= activityTokenAmount

        buyerGetsActivityToken :: Bool
        buyerGetsActivityToken
          = boostOf (valuePaidTo info buyerPkh) >= activityTokenAmount

        isActive :: Bool
        isActive = case siCloseInfo l of
          Just CloseInfo { ciTimeout = Just timeout} ->
            timeout `after` txInfoValidRange
          _ -> True

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

        boostWentToMarketplace :: Bool
        boostWentToMarketplace = boostOf scriptInput <= boostOf (valuePaidTo info (siBoostPayoutPkh l))

      in TRACE_IF_FALSE("Outputs are invalid!", outputsAreValid)
      && TRACE_IF_FALSE("Input value is incorrect", scriptInputIsValid)
      && TRACE_IF_FALSE("Expired", isActive)
      && TRACE_IF_FALSE(
                "Seller did not get activity token",
                sellerGetsActivityToken)
      && TRACE_IF_FALSE(
                "Buyer did not get activity token",
                buyerGetsActivityToken)
      && TRACE_IF_FALSE("Boost did not go to the marketplace",
          boostWentToMarketplace)

    Close -> case siCloseInfo l of
      Just CloseInfo {ciTimeout = Just timeout, ..} ->
        let
          assetsReturnedToOwner :: Bool
          assetsReturnedToOwner = paidAtleastTo info (siOwner l) ciValue

          isExpired :: Bool
          isExpired = timeout `before` txInfoValidRange

          scriptInput :: Value
          scriptInput = getOnlyScriptInput info

          scriptInputIsValid :: Bool
          scriptInputIsValid = scriptInput `geq` ciValue

        in TRACE_IF_FALSE("Assets not returned to owner", assetsReturnedToOwner)
        && TRACE_IF_FALSE("Not expired", isExpired)
        && TRACE_IF_FALSE("Input value is incorrect", scriptInputIsValid)
      _ -> TRACE_ERROR("Not configured for expiration")
    EmergencyClose ->  case siCloseInfo l of
      Just CloseInfo {ciEmergencyCloser = Just emergencyCloser, ..} ->
        let
          signedByEmergencyCloser :: Bool
          signedByEmergencyCloser = info `txSignedBy` emergencyCloser

          assetsReturnedToOwner :: Bool
          assetsReturnedToOwner = paidAtleastTo info (siOwner l) ciValue

          scriptInput :: Value
          scriptInput = getOnlyScriptInput info

          scriptInputIsValid :: Bool
          scriptInputIsValid = scriptInput `geq` ciValue

        in TRACE_IF_FALSE("Not signed by NFT closer", signedByEmergencyCloser)
        && TRACE_IF_FALSE("Assets not returned to owner", assetsReturnedToOwner)
        && TRACE_IF_FALSE("Input value is incorrect", scriptInputIsValid)
      _ -> TRACE_ERROR("Not configured for emergency close")

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
  . SBS.toShort
  . LB.toStrict
  $ serialise
    validator
