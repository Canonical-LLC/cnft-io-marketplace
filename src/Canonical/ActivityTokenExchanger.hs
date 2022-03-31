module Canonical.ActivityTokenExchanger
  ( exchanger
  , exchangerHash
  ) where
import           PlutusTx.Prelude
-- import           Plutus.V1.Ledger.Credential
import           Ledger
import           PlutusTx
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Codec.Serialise
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Scripts
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Canonical.Shared
-- import qualified PlutusTx.AssocMap as M
-- import           Ledger.Value
#include "DebugUtilities.h"

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data ExchangerLockerInput = ExchangerInput

data ExchangerUnlockerAction
  = Cancel
  | Unlock

PlutusTx.unstableMakeIsData ''ExchangerLockerInput
PlutusTx.unstableMakeIsData ''ExchangerUnlockerAction

validateExchanger
  :: ExchangerLockerInput
  -> ExchangerUnlockerAction
  -> ScriptContext
  -> Bool
validateExchanger = error ()

wrapValidateExchanger
    :: BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateExchanger = wrap validateExchanger

exchangerValidator :: Scripts.Validator
exchangerValidator = Scripts.mkValidatorScript
    $$(PlutusTx.compile [|| wrapValidateExchanger ||])

exchangerHash :: ValidatorHash
exchangerHash = validatorHash exchangerValidator

exchanger :: PlutusScript PlutusScriptV1
exchanger
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  $ serialise
    exchangerValidator
