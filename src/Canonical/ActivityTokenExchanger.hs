module Canonical.ActivityTokenExchanger
  ( ExchangerConfig (..)
  , EscrowInput (..)
  , ExchangerLockerInput (..)
  , TokenCounter (..)
  , exchanger
  , exchangerHash
  , globalNft
  , globalNftPolicyId
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
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Canonical.Shared
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
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

instance Eq TokenCounter where
  x == y
   =  tcCount             x == tcCount             y
   && tcActivityTokenName x == tcActivityTokenName y
   && tcActivityPolicyId  x == tcActivityPolicyId  y

type ExchangerUnlockerAction = BuiltinData

PlutusTx.unstableMakeIsData ''EscrowInput
PlutusTx.unstableMakeIsData ''TokenCounter
PlutusTx.unstableMakeIsData ''ExchangerLockerInput
makeLift ''ExchangerConfig

convertInputsThisScriptOnly
  :: UnsafeFromData a
  => [TxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(a, Value)]
convertInputsThisScriptOnly ins datums vh = go [] ins  where
  go acc = \case
    [] -> acc
    TxInInfo
      {txInInfoResolved = TxOut
        { txOutDatumHash = mdh
        , txOutAddress = Address {..}
        , txOutValue
        }
      }:xs ->

        if ScriptCredential vh == addressCredential then
          case mdh of
            Just dh ->
              go  ( ( unsafeFromBuiltinData (extractDatumBytes datums dh)
                    , txOutValue
                    )
                  : acc
                  )
                  xs
            Nothing -> TRACE_ERROR("Script input missing datum hash")
        else
          TRACE_ERROR("Unexpected script input")

partitionStep :: (ExchangerLockerInput, Value)
              -> (Maybe (TokenCounter, Value), [(PubKeyHash, Value)])
              -> (Maybe (TokenCounter, Value), [(PubKeyHash, Value)])
partitionStep (d, v) (mcounter, xs) = case (d, mcounter) of
  (ELI_EscrowInput EscrowInput {..}, _) -> (mcounter, (eiOwner, v) : xs)
  (ELI_TokenCounter tc, Nothing) -> (Just (tc, v), xs)
  _ -> TRACE_ERROR("Already found one counter datum")


partitionInputs :: [(ExchangerLockerInput, Value)] -> ((TokenCounter, Value), [(PubKeyHash, Value)])
partitionInputs xs = case foldr partitionStep (Nothing, []) xs of
  (Just x, xs') -> (x, xs')
  _ -> TRACE_ERROR("No counter datum")

-- Get index on nft
-- Get Token from nft
-- Collect all script input datums and values
-- assert only this type of script
validateExchanger
  :: ExchangerConfig
  -> BuiltinData
  -> BuiltinData
  -> ScriptContext
  -> Bool
validateExchanger ExchangerConfig {..} _ _ ctx@ScriptContext
  { scriptContextTxInfo = info@TxInfo {..}
  } =
  let
    thisValidator :: ValidatorHash
    thisValidator = ownHash ctx

    scriptInputs :: [(ExchangerLockerInput, Value)]
    scriptInputs = convertInputsThisScriptOnly txInfoInputs txInfoData thisValidator

    oldCounter :: TokenCounter
    counterValue :: Value
    allEscrowInputs :: [(PubKeyHash, Value)]

    ((oldCounter, counterValue), allEscrowInputs) = partitionInputs scriptInputs

    hasNFT :: Value -> Bool
    hasNFT v = M.member ecGlobalCounterNft (getValue v)

    hasCorrectNFTInput :: Bool
    hasCorrectNFTInput = hasNFT counterValue

    tokenConversionRate :: Integer
    tokenConversionRate
      = max 0
      $ ((ecRateNumerator * tcCount oldCounter) `divide` ecRateDenominator)
      + ecInitialAmount

    outputDatum :: ExchangerLockerInput
    outputValue :: Value

    (outputDatum, outputValue) = case getContinuingOutputs ctx of
      [TxOut { .. }] -> case txOutDatumHash of
          Just dh -> (extractData txInfoData dh, txOutValue)
          Nothing -> TRACE_ERROR("Missing Datum Hash")
      _ -> TRACE_ERROR("Wrong number of continuing outputs")

    hasCorrectNFTOutputValue :: Bool
    hasCorrectNFTOutputValue = hasNFT outputValue

    hasCorrectNFTOutputDatum :: Bool
    hasCorrectNFTOutputDatum = case outputDatum of
      ELI_EscrowInput  _ -> False
      ELI_TokenCounter newCounter -> newCounter == oldCounter
        { tcCount = tcCount oldCounter + 1
        }

    activityTokensOf :: Value -> Integer
    activityTokensOf v = valueOf v (tcActivityPolicyId oldCounter) (tcActivityTokenName oldCounter)

    allEscrowActivityTokens :: [(PubKeyHash, Integer)]
    allEscrowActivityTokens = map (\(p, x) -> (p, activityTokensOf x)) allEscrowInputs

    convertActivityTokens :: Integer -> Integer
    convertActivityTokens v = v * tokenConversionRate

    updatePaymentMap :: (PubKeyHash, Integer)
                     -> Map PubKeyHash Integer
                     -> Map PubKeyHash Integer
    updatePaymentMap (pkh, v) =
      M.unionWith (+) (M.singleton pkh (convertActivityTokens v))

    allOwners :: Map PubKeyHash Integer
    allOwners = foldr updatePaymentMap M.empty allEscrowActivityTokens

    tokensOf :: Value -> Integer
    tokensOf v = valueOf v ecPolicyId ecTokenName

    isPaidTokens :: (PubKeyHash, Integer) -> Bool
    isPaidTokens (pkh, tokenCount) = tokensOf (valuePaidTo info pkh) > tokenCount

    fundsGoToAllOwners :: Bool
    fundsGoToAllOwners = all isPaidTokens $ M.toList allOwners

    amountToBurn :: Integer
    amountToBurn
      = negate
      $ sum
      $ map snd allEscrowActivityTokens

    allActivityTokensAreBurned :: Bool
    allActivityTokensAreBurned
      = activityTokensOf txInfoMint == amountToBurn

    totalTokensPaid :: Integer
    totalTokensPaid
      = sum
      $ map snd
      $ M.toList allOwners

    outputTokensAreCorrect :: Bool
    outputTokensAreCorrect
      = tokensOf counterValue - totalTokensPaid < tokensOf outputValue

  in TRACE_IF_FALSE("NFT input not correct", hasCorrectNFTInput)
  && TRACE_IF_FALSE("NFT output not correct value", hasCorrectNFTOutputValue)
  && TRACE_IF_FALSE("NFT output not correct datum", hasCorrectNFTOutputDatum)
  && TRACE_IF_FALSE("Not all funds disbursed", fundsGoToAllOwners)
  && TRACE_IF_FALSE("Burn Activity Tokens", allActivityTokensAreBurned)
  && TRACE_IF_FALSE("Funds are not returned script", outputTokensAreCorrect)

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
