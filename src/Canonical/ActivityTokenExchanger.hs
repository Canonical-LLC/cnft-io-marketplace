module Canonical.ActivityTokenExchanger
  ( ExchangerConfig (..)
  , EscrowInput (..)
  , TokenCounter (..)
  , exchanger
  , exchangerHash
  , globalNft
  , globalNftPolicyId
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
import           Ledger.Value
#include "DebugUtilities.h"

hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken v c t = valueOf v c t == 1

mkNftMinter :: TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNftMinter theTokenName utxo _ ctx =
  let
    TxInfo{..} = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) txInfoInputs

    theCurrencySymbol :: CurrencySymbol
    theCurrencySymbol = ownCurrencySymbol ctx

    hasSingleNft :: Value -> Bool
    hasSingleNft v = hasSingleToken v theCurrencySymbol theTokenName

    onlyOneTokenMinted :: Bool
    onlyOneTokenMinted = hasSingleNft txInfoMint

    outputIndexIsZero :: Bool
    outputIndexIsZero = case filter (\TxOut {..} -> hasSingleNft txOutValue) txInfoOutputs of
      [TxOut {..}] -> case txOutDatumHash of
        Nothing -> TRACE_ERROR("No datum hash")
        Just dh -> case PlutusTx.fromBuiltinData (getDatum (extractDatum txInfoData dh)) of
          Nothing -> TRACE_ERROR("Failed to convert datum")
          Just (ELI_TokenCounter TokenCounter {..})
            -> TRACE_IF_FALSE("Wager index is not zero", (tcCount == 0))
            && TRACE_IF_FALSE("Policy id of nft is wrong", (tcActivityPolicyId == theCurrencySymbol))
            && TRACE_IF_FALSE("Token name of nft is wrong", (tcActivityTokenName == theTokenName))
          _ -> TRACE_ERROR("Wrong type of datum")
      _ -> TRACE_ERROR("Impossible. No minted output.")

  in TRACE_IF_FALSE("Missing significant UTxO!", hasUTxO)
  && TRACE_IF_FALSE("Wrong mint amount!"       , onlyOneTokenMinted)
  && TRACE_IF_FALSE("Output index is not zero" , outputIndexIsZero)

nftPolicy :: TokenName -> TxOutRef -> Scripts.MintingPolicy
nftPolicy theTokenName utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \theTokenName' utxo' -> Scripts.wrapMintingPolicy $ mkNftMinter theTokenName' utxo' ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode theTokenName
    `PlutusTx.applyCode`
     PlutusTx.liftCode utxo

globalNftPolicyId :: TokenName -> TxOutRef -> CurrencySymbol
globalNftPolicyId theTokenName = mpsSymbol . mintingPolicyHash . nftPolicy theTokenName

globalNft :: TokenName -> TxOutRef -> PlutusScript PlutusScriptV1
globalNft theTokenName
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . nftPolicy theTokenName

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data ExchangerConfig = ExchangerConfig
  { ecRateNumerator    :: Integer
  , ecRateDenominator  :: Integer
  , ecInitialAmount    :: Integer
  , ecTokenName        :: TokenName
  , ecPolicyId         :: CurrencySymbol
  , ecGlobalCounterNft :: CurrencySymbol
  }

data ExchangerLockerInput
  = ELI_EscrowInput  EscrowInput
  | ELI_TokenCounter TokenCounter

data EscrowInput = EscrowInput
  { eiOwner :: PubKeyHash
  }

data TokenCounter = TokenCounter
  { tcCount             :: Integer
  , tcActivityTokenName :: TokenName
  , tcActivityPolicyId  :: CurrencySymbol
  }

type ExchangerUnlockerAction = BuiltinData

PlutusTx.unstableMakeIsData ''EscrowInput
PlutusTx.unstableMakeIsData ''TokenCounter
PlutusTx.unstableMakeIsData ''ExchangerLockerInput
makeLift ''ExchangerConfig

-- Get index on nft
-- Get Token from nft
-- Collect all script input datums and values
-- assert only this type of script
-- get all of the activity tokens by user
-- convert to output token per pkh
validateExchanger
  :: ExchangerConfig
  -> ExchangerLockerInput
  -> ExchangerUnlockerAction
  -> ScriptContext
  -> Bool
validateExchanger = error ()

wrapValidateExchanger
    :: ExchangerConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateExchanger = wrap . validateExchanger

exchangerValidator :: ExchangerConfig -> Scripts.Validator
exchangerValidator config = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateExchanger ||])
    `applyCode`
    liftCode config

exchangerHash :: ExchangerConfig -> ValidatorHash
exchangerHash = validatorHash . exchangerValidator

exchanger :: ExchangerConfig -> PlutusScript PlutusScriptV1
exchanger
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . exchangerValidator
