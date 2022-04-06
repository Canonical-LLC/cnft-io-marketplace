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

data ExchangerAddress = ExchangerAddress
  { aaddressCredential        :: Credential
  , aaddressStakingCredential :: BuiltinData
  }

data ExchangerTxOut = ExchangerTxOut
  { atxOutAddress             :: ExchangerAddress
  , atxOutValue               :: Value
  , atxOutDatumHash           :: Maybe DatumHash
  }

data ExchangerTxInInfo = ExchangerTxInInfo
  { atxInInfoOutRef           :: TxOutRef
  , atxInInfoResolved         :: ExchangerTxOut
  }

data ExchangerTxInfo = ExchangerTxInfo
  { atxInfoInputs             :: [ExchangerTxInInfo]
  , atxInfoOutputs            :: [ExchangerTxOut]
  , atxInfoFee                :: BuiltinData
  , atxInfoMint               :: Value
  , atxInfoDCert              :: BuiltinData
  , atxInfoWdrl               :: BuiltinData
  , atxInfoValidRange         :: BuiltinData
  , atxInfoSignatories        :: BuiltinData
  , atxInfoData               :: [(DatumHash, Datum)]
  , atxInfoId                 :: BuiltinData
  }

data ExchangerScriptPurpose
    = ASpending TxOutRef

data ExchangerScriptContext = ExchangerScriptContext
  { aScriptContextTxInfo  :: ExchangerTxInfo
  , aScriptContextPurpose :: ExchangerScriptPurpose
  }



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
  , tcActivityPolicyId  :: CurrencySymbol
  , tcActivityTokenName :: TokenName
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

makeIsDataIndexed  ''ExchangerScriptPurpose [('ASpending,1)]
unstableMakeIsData ''ExchangerTxInfo
unstableMakeIsData ''ExchangerScriptContext
unstableMakeIsData ''ExchangerAddress
unstableMakeIsData ''ExchangerTxOut
unstableMakeIsData ''ExchangerTxInInfo

convertInputsThisScriptOnly
  :: UnsafeFromData a
  => [ExchangerTxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(a, TxOutRef,  Value)]
convertInputsThisScriptOnly ins datums vh = go [] ins  where
  go acc = \case
    [] -> acc
    ExchangerTxInInfo
      {atxInInfoResolved = ExchangerTxOut
        { atxOutDatumHash = mdh
        , atxOutAddress = ExchangerAddress {..}
        , atxOutValue
        }
      , ..
      }:xs ->
        case aaddressCredential of
          ScriptCredential vh'
            | vh' == vh ->
              case mdh of
                Just dh ->
                  go  ( ( unsafeFromBuiltinData (extractDatumBytes datums dh)
                        , atxInInfoOutRef
                        , atxOutValue
                        )
                      : acc
                      )
                      xs
                Nothing -> TRACE_ERROR_EXCHANGE("Script input missing datum hash")
            | otherwise -> TRACE_ERROR_EXCHANGE("Unexpected script input")
          _ -> go acc xs

partitionStep :: (ExchangerLockerInput, TxOutRef, Value)
              -> (Maybe (TokenCounter, TxOutRef, Value), [(PubKeyHash, Value)])
              -> (Maybe (TokenCounter, TxOutRef, Value), [(PubKeyHash, Value)])
partitionStep (d, ref, v) (mcounter, xs) = case (d, mcounter) of
  (ELI_EscrowInput EscrowInput {..}, _) -> (mcounter, (eiOwner, v) : xs)
  (ELI_TokenCounter tc, Nothing) -> (Just (tc, ref, v), xs)
  _ -> TRACE_ERROR_EXCHANGE("Already found one counter datum")

partitionInputs :: [(ExchangerLockerInput, TxOutRef, Value)] -> ((TokenCounter, TxOutRef, Value), [(PubKeyHash, Value)])
partitionInputs xs = case foldr partitionStep (Nothing, []) xs of
  (Just x, xs') -> (x, xs')
  _ -> TRACE_ERROR_EXCHANGE("No counter datum")

findDatum'
  :: PlutusTx.UnsafeFromData a
  => [(DatumHash, Datum)]
  -> DatumHash
  -> a
findDatum' info dh
  = PlutusTx.unsafeFromBuiltinData
    (getDatum
     (extractDatum info dh))

getContinuingOutputs'
  :: PlutusTx.UnsafeFromData a
  => [(DatumHash, Datum)]
  -> ValidatorHash
  -> [ExchangerTxOut]
  -> [(a, Value)]
getContinuingOutputs' datums vh outs = go [] outs where
  go acc = \case
    [] -> acc
    ExchangerTxOut
      { atxOutDatumHash = Just dh
      , atxOutAddress = ExchangerAddress
          { aaddressCredential = ScriptCredential vh'
          }
      , atxOutValue
      }:os
        | vh == vh' -> go ((findDatum' datums dh, atxOutValue) : acc) os
        | otherwise -> go acc os
    _ : os -> go acc os

ownHash' :: [ExchangerTxInInfo] -> TxOutRef -> ValidatorHash
ownHash' ins txOutRef = go ins where
    go = \case
      [] -> TRACE_ERROR("The impossible happened")
      ExchangerTxInInfo {..} :xs ->
        if atxInInfoOutRef == txOutRef then
          case atxOutAddress atxInInfoResolved of
            ExchangerAddress (ScriptCredential s) _ -> s
            _ -> TRACE_ERROR("The impossible happened")
        else
          go xs

valuePaidTo' :: [ExchangerTxOut] -> PubKeyHash -> Value
valuePaidTo' outs pkh = mconcat (pubKeyOutputsAt' pkh outs)

pubKeyOutputsAt' :: PubKeyHash -> [ExchangerTxOut] -> [Value]
pubKeyOutputsAt' pk outs =
    let flt ExchangerTxOut { atxOutAddress = ExchangerAddress (PubKeyCredential pk') _, atxOutValue }
          | pk == pk' = Just atxOutValue
          | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt outs


-- Get index on nft
-- Get Token from nft
-- Collect all script input datums and values
-- assert only this type of script
validateExchanger
  :: ExchangerConfig
  -> BuiltinData
  -> BuiltinData
  -> ExchangerScriptContext
  -> Bool
validateExchanger ExchangerConfig {..} _ _ ExchangerScriptContext
  { aScriptContextTxInfo = ExchangerTxInfo {..}
  , aScriptContextPurpose = ASpending thisTxOutRef
  } =
  let
    thisValidator :: ValidatorHash
    thisValidator = ownHash' atxInfoInputs thisTxOutRef

    scriptInputs :: [(ExchangerLockerInput, TxOutRef, Value)]
    scriptInputs = convertInputsThisScriptOnly atxInfoInputs atxInfoData thisValidator

    oldCounter :: TokenCounter
    counterValue :: Value
    allEscrowInputs :: [(PubKeyHash, Value)]

    ((oldCounter, counterRef, counterValue), allEscrowInputs) = partitionInputs scriptInputs

  in if counterRef /= thisTxOutRef then
      True
    else
      let
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

        (outputDatum, outputValue) = case getContinuingOutputs' atxInfoData thisValidator atxInfoOutputs of
          [(d, v)] -> (d, v)
          _ -> TRACE_ERROR_EXCHANGE("Wrong number of continuing outputs")

        hasCorrectNFTOutputValue :: Bool
        hasCorrectNFTOutputValue = hasNFT outputValue

        activityTokensOf :: Value -> Integer
        activityTokensOf v = valueOf v (tcActivityPolicyId oldCounter) (tcActivityTokenName oldCounter)

        allEscrowActivityTokens :: [(PubKeyHash, Integer)]
        allEscrowActivityTokens = map (\(p, x) -> (p, activityTokensOf x)) allEscrowInputs

        amountToBurn :: Integer
        amountToBurn = go 0 allEscrowActivityTokens where
          go acc = \case
            [] -> acc
            (_, x) : xs -> go (acc + x) xs

        hasCorrectNFTOutputDatum :: Bool
        hasCorrectNFTOutputDatum = case outputDatum of
          ELI_EscrowInput  _ -> False
          ELI_TokenCounter newCounter -> newCounter == oldCounter
            { tcCount = tcCount oldCounter + amountToBurn
            }

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
        isPaidTokens (pkh, tokenCount) = go 0 atxInfoOutputs >= tokenCount where
          go acc = \case
            [] -> acc
            ExchangerTxOut
              { atxOutAddress = ExchangerAddress
                  {aaddressCredential = PubKeyCredential pkh'}
              , ..
              } : xs
                | pkh' == pkh -> go (tokensOf atxOutValue + acc) xs
                | otherwise -> go acc xs
            _:xs -> go acc xs


        fundsGoToAllOwners :: Bool
        fundsGoToAllOwners = all isPaidTokens (M.toList allOwners)

        allActivityTokensAreBurned :: Bool
        allActivityTokensAreBurned
          = activityTokensOf atxInfoMint == negate amountToBurn

        totalTokensPaid :: Integer
        totalTokensPaid = go 0 (M.toList allOwners) where
          go acc = \case
            [] -> acc
            (_, x) : xs -> go (acc + x) xs

        outputTokensAreCorrect :: Bool
        outputTokensAreCorrect
          = tokensOf counterValue - totalTokensPaid <= tokensOf outputValue

      in TRACE_IF_FALSE_EXCHANGE("NFT input not correct", hasCorrectNFTInput)
      && TRACE_IF_FALSE_EXCHANGE("NFT output not correct value", hasCorrectNFTOutputValue)
      && TRACE_IF_FALSE_EXCHANGE("NFT output not correct datum", hasCorrectNFTOutputDatum)
      && TRACE_IF_FALSE_EXCHANGE("Not all funds disbursed", fundsGoToAllOwners)
      && TRACE_IF_FALSE_EXCHANGE("Burn Activity Tokens", allActivityTokensAreBurned)
      && TRACE_IF_FALSE_EXCHANGE("Funds are not returned script", outputTokensAreCorrect)

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
