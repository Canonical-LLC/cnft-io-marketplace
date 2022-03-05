module Canonical.Escrow
  ( EscrowLockerInput(..)
  , escrowValidatorHash
  , escrowScript
  ) where
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Credential
import           Ledger
import           PlutusTx
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Codec.Serialise
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Canonical.Shared

#define DEBUG

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#endif

data EscrowAddress = EscrowAddress
  { eAddressCredential        :: Credential
  , eAddressStakingCredential :: BuiltinData
  }

data EscrowTxOut = EscrowTxOut
  { etxOutAddress             :: EscrowAddress
  , etxOutValue               :: BuiltinData
  , etxOutDatumHash           :: BuiltinData
  }

data EscrowTxInInfo = EscrowTxInInfo
  { etxInInfoOutRef   :: BuiltinData
  , etxInInfoResolved :: EscrowTxOut
  }

data EscrowScriptContext = EscrowScriptContext
  { eScriptContextTxInfo  :: EscrowTxInfo
  , eScriptContextPurpose :: BuiltinData
  }

data EscrowTxInfo = EscrowTxInfo
  { etxInfoInputs             :: [EscrowTxInInfo]
  , etxInfoOutputs            :: BuiltinData
  , etxInfoFee                :: BuiltinData
  , etxInfoMint               :: BuiltinData
  , etxInfoDCert              :: BuiltinData
  , etxInfoWdrl               :: BuiltinData
  , etxInfoValidRange         :: BuiltinData
  , etxInfoSignatories        :: [PubKeyHash]
  , etxInfoData               :: BuiltinData
  , etxInfoId                 :: BuiltinData
  }

data EscrowLockerInput a = EscrowLockerInput
  { eliOwner              :: PubKeyHash
  , eliData               :: a
  , eliUnlockingValidator :: ValidatorHash
  }

data EscrowUnlockerAction
  = Cancel
  | Unlock

makeIsDataIndexed ''EscrowAddress [('EscrowAddress,0)]
makeIsDataIndexed ''EscrowTxOut [('EscrowTxOut,0)]
makeIsDataIndexed ''EscrowTxInInfo [('EscrowTxInInfo,0)]
makeIsDataIndexed ''EscrowScriptContext [('EscrowScriptContext,0)]
makeIsDataIndexed ''EscrowTxInfo [('EscrowTxInfo,0)]
makeIsDataIndexed ''EscrowLockerInput [('EscrowLockerInput,0)]
makeIsDataIndexed ''EscrowUnlockerAction [('Cancel,0), ('Unlock, 1)]

etxSignedBy :: EscrowTxInfo -> PubKeyHash -> Bool
etxSignedBy EscrowTxInfo {..} pkh = any (== pkh) etxInfoSignatories

validateEscrow
  :: EscrowLockerInput BuiltinData
  -> EscrowUnlockerAction
  -> EscrowScriptContext
  -> Bool
validateEscrow
  EscrowLockerInput {..}
  action
  EscrowScriptContext
    { eScriptContextTxInfo = info
    } = case action of
          Cancel -> info `etxSignedBy` eliOwner
          Unlock -> any (\EscrowTxInInfo
                            { etxInInfoResolved = EscrowTxOut
                              { etxOutAddress = EscrowAddress {..}
                              }
                            } ->
                              ScriptCredential eliUnlockingValidator == eAddressCredential
                        )
                        (etxInfoInputs info)

wrapValidateEscrow
    :: BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateEscrow = wrap validateEscrow

escrowValidator :: Scripts.Validator
escrowValidator = Scripts.mkValidatorScript
    $$(PlutusTx.compile [|| wrapValidateEscrow ||])

escrowValidatorHash :: ValidatorHash
escrowValidatorHash = validatorHash escrowValidator

escrowScript :: PlutusScript PlutusScriptV1
escrowScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  $ serialise
    escrowValidator
