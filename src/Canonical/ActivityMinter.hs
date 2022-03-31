module Canonical.ActivityMinter
  ( activityPolicyId
  , activity
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
import           Canonical.Shared
import qualified PlutusTx.AssocMap as M
#include "DebugUtilities.h"

data Action = A_Mint | A_Burn

PlutusTx.unstableMakeIsData ''Action

data ActivityAddress = ActivityAddress
  { aaddressCredential        :: Credential
  , aaddressStakingCredential :: BuiltinData
  }

data ActivityTxOut = ActivityTxOut
  { atxOutAddress             :: ActivityAddress
  , atxOutValue               :: Value
  , atxOutDatumHash           :: Maybe DatumHash
  }

data ActivityTxInInfo = ActivityTxInInfo
  { atxInInfoOutRef           :: BuiltinData
  , atxInInfoResolved         :: ActivityTxOut
  }

data ActivityTxInfo = ActivityTxInfo
  { atxInfoInputs             :: [ActivityTxInInfo]
  , atxInfoOutputs            :: BuiltinData
  , atxInfoFee                :: BuiltinData
  , atxInfoMint               :: Value
  , atxInfoDCert              :: BuiltinData
  , atxInfoWdrl               :: BuiltinData
  , atxInfoValidRange         :: BuiltinData
  , atxInfoSignatories        :: BuiltinData
  , atxInfoData               :: BuiltinData
  , atxInfoId                 :: BuiltinData
  }

data ActivityScriptPurpose
    = AMinting CurrencySymbol

data ActivityScriptContext = ActivityScriptContext
  { aScriptContextTxInfo  :: ActivityTxInfo
  , aScriptContextPurpose :: ActivityScriptPurpose
  }

PlutusTx.unstableMakeIsData ''ActivityAddress
PlutusTx.unstableMakeIsData ''ActivityTxOut
PlutusTx.unstableMakeIsData ''ActivityTxInInfo
PlutusTx.unstableMakeIsData ''ActivityTxInfo
PlutusTx.unstableMakeIsData ''ActivityScriptPurpose
PlutusTx.unstableMakeIsData ''ActivityScriptContext

activityTokenName :: TokenName
activityTokenName = "ACTIVITY"

mkPolicy :: TokenName -> [ValidatorHash] -> Action -> ActivityScriptContext -> Bool
mkPolicy tName vhs action ActivityScriptContext
    { aScriptContextTxInfo  = ActivityTxInfo {..}
    , aScriptContextPurpose = AMinting theCurrencySymbol
    } = case action of
  A_Burn -> case M.lookup theCurrencySymbol (getValue atxInfoMint) of
          Nothing -> TRACE_ERROR("Not burning this policy id")
          Just m  -> case M.lookup tName m of
            Just c -> c < 0
            _ -> TRACE_ERROR("Must burn this policy")
  A_Mint ->
    let
      isUnlockingValidator :: ActivityTxInInfo -> Bool
      isUnlockingValidator = \case
        ActivityTxInInfo
          { atxInInfoResolved = ActivityTxOut
              { atxOutAddress = ActivityAddress
                { aaddressCredential = ScriptCredential vh
                }
              }
          } -> any (vh==) vhs
        _ -> False

      hasUnlockingValidator :: Bool
      hasUnlockingValidator = any isUnlockingValidator atxInfoInputs

    in TRACE_IF_FALSE("Missing validator", hasUnlockingValidator)

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------
wrappedPolicy :: TokenName -> [ValidatorHash] ->  WrappedMintingPolicyType
wrappedPolicy t vhs = wrapMint $ mkPolicy t vhs

policy :: [ValidatorHash] -> MintingPolicy
policy vhs = mkMintingPolicyScript $
  $$(compile [|| \t vhs' -> wrappedPolicy t vhs' ||])
  `applyCode`
  liftCode activityTokenName
  `applyCode`
  liftCode vhs

plutusScript :: [ValidatorHash] -> Script
plutusScript = unMintingPolicyScript . policy

validator :: [ValidatorHash] -> Validator
validator = Validator . plutusScript

activityPolicyId :: [ValidatorHash] -> CurrencySymbol
activityPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: [ValidatorHash] ->  LB.ByteString
scriptAsCbor = serialise . validator

activity :: [ValidatorHash] -> PlutusScript PlutusScriptV1
activity
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . scriptAsCbor
