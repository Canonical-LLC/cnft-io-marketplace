module Canonical.Auction
  ( auctionScript
  , auctionScriptHash
  , Auction(..)
  , Action(..)
  , payoutPerAddress
  , mergeSort
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import           Canonical.Escrow
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           PlutusTx
import           PlutusTx.Prelude
import           Ledger.Value
import           Plutus.V1.Ledger.Credential
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Canonical.Shared
import           Canonical.BidMinter
import qualified Ledger.Ada as A
#include "DebugUtilities.h"

-------------------------------------------------------------------------------
-- Custom ScriptContext types to improvement transaction size and memory usage
-------------------------------------------------------------------------------
data AuctionAddress = AuctionAddress
  { aaddressCredential        :: Credential
  , aaddressStakingCredential :: BuiltinData
  }

data AuctionTxOut = AuctionTxOut
  { atxOutAddress             :: AuctionAddress
  , atxOutValue               :: Value
  , atxOutDatumHash           :: Maybe DatumHash
  }

data AuctionTxInInfo = AuctionTxInInfo
  { atxInInfoOutRef           :: TxOutRef
  , atxInInfoResolved         :: AuctionTxOut
  }

data AuctionTxInfo = AuctionTxInfo
  { atxInfoInputs             :: [AuctionTxInInfo]
  , atxInfoOutputs            :: [AuctionTxOut]
  , atxInfoFee                :: BuiltinData
  , atxInfoMint               :: Value
  , atxInfoDCert              :: BuiltinData
  , atxInfoWdrl               :: BuiltinData
  , atxInfoValidRange         :: POSIXTimeRange
  , atxInfoSignatories        :: [PubKeyHash]
  , atxInfoData               :: [(DatumHash, Datum)]
  , atxInfoId                 :: BuiltinData
  }

data AuctionScriptPurpose
    = ASpending TxOutRef

data AuctionScriptContext = AuctionScriptContext
  { aScriptContextTxInfo  :: AuctionTxInfo
  , aScriptContextPurpose :: AuctionScriptPurpose
  }

ownHash' :: [AuctionTxInInfo] -> TxOutRef -> ValidatorHash
ownHash' ins txOutRef = go ins where
    go = \case
      [] -> TRACE_ERROR("The impossible happened")
      AuctionTxInInfo {..} :xs ->
        if atxInInfoOutRef == txOutRef then
          case atxOutAddress atxInInfoResolved of
            AuctionAddress (ScriptCredential s) _ -> s
            _ -> TRACE_ERROR("The impossible happened")
        else
          go xs

valuePaidTo' :: [AuctionTxOut] -> PubKeyHash -> Value
valuePaidTo' outs pkh = mconcat (pubKeyOutputsAt' pkh outs)

pubKeyOutputsAt' :: PubKeyHash -> [AuctionTxOut] -> [Value]
pubKeyOutputsAt' pk outs =
    let flt AuctionTxOut{ atxOutAddress = AuctionAddress (PubKeyCredential pk') _, atxOutValue }
          | pk == pk' = Just atxOutValue
          | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt outs

getContinuingOutputs'
  :: [(DatumHash, Datum)]
  -> ValidatorHash
  -> [AuctionTxOut]
  -> [(Auction, Value)]
getContinuingOutputs' datums vh outs = go [] outs where
  go acc = \case
    [] -> acc
    AuctionTxOut {..} : xs
      | aaddressCredential atxOutAddress == ScriptCredential vh ->
        case atxOutDatumHash of
          Just dh -> go ((extractData datums dh, atxOutValue):acc) xs
          Nothing -> TRACE_ERROR("Missing Datum Hash")
      | otherwise -> go acc xs

convertInputs'
  :: [AuctionTxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(BidEscrowLockerInput, Value)]
convertInputs' ins datums vh = go [] ins  where
  go acc = \case
    [] -> acc
    AuctionTxInInfo
      {atxInInfoResolved = AuctionTxOut
        { atxOutDatumHash = mdh
        , atxOutAddress = AuctionAddress {..}
        , atxOutValue
        }
      }:xs ->

        if ScriptCredential vh == aaddressCredential then
          case mdh of
            Just dh ->
              go  ( ( unsafeFromBuiltinData (extractDatumBytes datums dh)
                    , atxOutValue
                    )
                  : acc
                  )
                  xs
            Nothing -> TRACE_ERROR("Script input missing datum hash")
        else
          go acc xs
-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
type BidEscrowLockerInput = EscrowLockerInput BidData

data Auction = Auction
  { aSeller            :: PubKeyHash
  , aDeadline          :: POSIXTime
  , aBatcherDeadline   :: POSIXTime
  , aMinBid            :: Integer
  , aPayoutPercentages :: (M.Map PubKeyHash Integer)
  , aHighBid           :: (Maybe Bid)
  , aEscrowValidator   :: ValidatorHash
  , aValue             :: Value
  , aBidMinterPolicyId :: CurrencySymbol
  }

data Action = CollectBids | Close

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Auction where
  {-# INLINABLE (==) #-}
  x == y
    =  (aSeller            x == aSeller            y)
    && (aDeadline          x == aDeadline          y)
    && (aBatcherDeadline   x == aBatcherDeadline   y)
    && (aMinBid            x == aMinBid            y)
    && (aPayoutPercentages x == aPayoutPercentages y)
    && (aHighBid           x == aHighBid           y)
    && (aEscrowValidator   x == aEscrowValidator   y)
    && (aValue             x == aValue             y)
    && (aBidMinterPolicyId x == aBidMinterPolicyId y)

makeIsDataIndexed  ''AuctionScriptPurpose [('ASpending,1)]
unstableMakeIsData ''AuctionTxInfo
unstableMakeIsData ''AuctionScriptContext
unstableMakeIsData ''AuctionAddress
unstableMakeIsData ''AuctionTxOut
unstableMakeIsData ''AuctionTxInInfo
unstableMakeIsData ''Auction
unstableMakeIsData ''Action

-------------------------------------------------------------------------------
-- Sorting Utilities
-------------------------------------------------------------------------------
drop :: Integer -> [a] -> [a]
drop n l@(_:xs) =
    if n <= 0 then l
    else drop (n-1) xs
drop _ [] = []

merge :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
merge as@(a:as') bs@(b:bs') =
    if snd a <= snd b
    then a:(merge as'  bs)
    else b:(merge as bs')
merge [] bs = bs
merge as [] = as

mergeSort :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
mergeSort xs =
    let n = length xs
    in if n > 1
       then let n2 = n `divide` 2
            in merge (mergeSort (take n2 xs)) (mergeSort (drop n2 xs))
       else xs

-------------------------------------------------------------------------------
-- Payout Utilities
-------------------------------------------------------------------------------
type Percent = Integer
type Lovelaces = Integer


lovelacesPaidTo :: [AuctionTxOut] -> PubKeyHash -> Integer
lovelacesPaidTo info pkh = go 0 info where
  go c = \case
    [] -> c
    AuctionTxOut{atxOutAddress = AuctionAddress (PubKeyCredential pkh') _, ..}:xs
     | pkh == pkh' -> go (lovelaces atxOutValue + c) xs
     | otherwise   -> go c xs
    _ : xs -> go c xs

minAda :: Lovelaces
minAda = 1_000_000

sortPercents :: M.Map PubKeyHash Percent -> [(PubKeyHash, Percent)]
sortPercents = mergeSort . M.toList

-- This computes the payout by attempting to the honor the percentage while
-- keeping the payout above 1 Ada. Because 1 Ada could be higher than the
-- percentage, if a minimum occurs, it has to adjust the rest of the payouts
-- It does this by subtracting the amount payed from the total payout,
-- and subtracting the total percentage available after each payout.
-- More details are explained in the README. It is also the main
-- function tested in the unit tests.
--
-- !!!!!! IMPORTANT
-- This function assumes the input is sorted by percent from least to
-- greatest.
--
payoutPerAddress :: Integer -> [(PubKeyHash, Percent)] -> [(PubKeyHash, Lovelaces)]
payoutPerAddress total percents = go total 1000 percents where
  go left totalPercent = \case
    [] -> TRACE_ERROR("No seller percentage specified")
    [(pkh, _)] -> [(pkh, left)]
    (pkh, percent) : rest ->
      let !percentOfPot = applyPercent totalPercent left percent
          !percentOf = max minAda percentOfPot
      in (pkh, percentOf) : go (left - percentOf) (totalPercent - percent) rest

applyPercent :: Integer -> Lovelaces -> Percent -> Lovelaces
applyPercent divider inVal pct = (inVal * pct) `divide` divider

-- Sort the payout map from least to greatest.
-- Compute the payouts for each address.
-- Check that each address has received their payout.
payoutIsValid :: Lovelaces -> [AuctionTxOut] -> M.Map PubKeyHash Percent -> Bool
payoutIsValid total info
  = all (paidApplyPercent info)
  . payoutPerAddress total
  . sortPercents

-- For a given address and percentage pair, verify
-- they received greater or equal to their percentage
-- of the input.
paidApplyPercent :: [AuctionTxOut] -> (PubKeyHash, Lovelaces) -> Bool
paidApplyPercent info (addr, owed)
  = lovelacesPaidTo info addr >= owed

-------------------------------------------------------------------------------
{-

Batch Transaction Exploit Protection

All combinations of redeemers
outbid/outbid, close/close and close,outbid
are exploitable.

If an attacker outbids the same payout address twice,
they could rewire one of the bid refunds to themselves.

If an attacker outbids and closes with the same payout address,
they could rewire half the refunds/payouts.

If an attacker double closes with the same payout address,
they could rewire half the payouts.

-}
-------------------------------------------------------------------------------

isScriptCredential :: Credential -> Bool
isScriptCredential = \case
  ScriptCredential _ -> True
  _ -> False

expectedScripts :: [AuctionTxInInfo] -> ValidatorHash -> ValidatorHash -> Maybe Value
expectedScripts theInputs auctionValidator escrowValidator =
  let
    auctionCredential :: Credential
    auctionCredential = ScriptCredential auctionValidator

    escrowCredential :: Credential
    escrowCredential = ScriptCredential escrowValidator

    go mValue = \case
      []   -> mValue
      AuctionTxInInfo { atxInInfoResolved = AuctionTxOut {atxOutAddress = AuctionAddress {..}, ..}}:xs ->
        if aaddressCredential == auctionCredential then
          case mValue of
            Nothing -> go (Just atxOutValue) xs
            Just _ -> TRACE_ERROR("More than one auction input")
        else if aaddressCredential == escrowCredential then
          go mValue xs
        else if isScriptCredential aaddressCredential then
          TRACE_ERROR("bad script input")
        else
          go mValue xs

    in go Nothing theInputs


-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
mergeBids :: [Bid] -> Map PubKeyHash Integer
mergeBids
  = foldl
      (\ !acc Bid {..}
        -> M.unionWith (+) (M.singleton bidBidder bidAmount) acc
      )
      M.empty

bidIsPaid :: [AuctionTxOut] -> (PubKeyHash, Integer) -> Bool
bidIsPaid theOutputs (theUser, amount)
  = lovelacesPaidTo theOutputs theUser >= amount

convertEscrowInputToBid :: POSIXTime -> BidEscrowLockerInput -> Bid
convertEscrowInputToBid deadline EscrowLockerInput {..} =
  if bdValidEndTime eliData < deadline then
    Bid
      { bidBidder = eliOwner
      , bidAmount = bdBid eliData
      }
  else
    TRACE_ERROR("bid expired")


{-
This is an auction validator. It is configured with an asset, reserve price,
seller, and expiration.

Until it is expired, bids are accepted if they are higher than the reserve
price or the last bid.

Once the time for the auction expires, either the asset can be sent back to
the seller (if there were no bids), or the asset is sent to the highest bidder
and the bid Ada is split to the addresses in the 'aPayoutPercentages'.

The payout amounts are determined by the percentages in the
'aPayoutPercentages' map.
-}
mkValidator :: Auction -> Action -> AuctionScriptContext -> Bool
mkValidator auction@Auction {..} action AuctionScriptContext
  { aScriptContextTxInfo = AuctionTxInfo {..}
  , aScriptContextPurpose = ASpending thisOutRef
  } =
  let
    -- The value we expect on the script input based on
    -- datum.
    expectedScriptValue :: Value
    expectedScriptValue = case aHighBid of
      Nothing -> aValue
      Just Bid{..} -> aValue <> A.lovelaceValueOf bidAmount

    thisValidator :: ValidatorHash
    thisValidator = ownHash' atxInfoInputs thisOutRef

    actualScriptValue :: Value
    actualScriptValue = case expectedScripts atxInfoInputs thisValidator aEscrowValidator of
      Nothing -> TRACE_ERROR("Wrong input scripts")
      Just x -> x

    -- Ensure the value is on the script address and there is
    -- only one script input.
    correctInputValue :: Bool
    correctInputValue = actualScriptValue `geq` expectedScriptValue

  -- Always perform the input check
  in TRACE_IF_FALSE("wrong input value", correctInputValue)
  && case action of
    CollectBids ->
      let
        escrowValidatorAsTokenName :: TokenName
        escrowValidatorAsTokenName = case aEscrowValidator of
          ValidatorHash vh -> TokenName vh

        bidTokenCount :: Value -> Integer
        bidTokenCount (Value v) = case M.lookup aBidMinterPolicyId v of
          Nothing -> 0
          Just m -> case M.toList m of
            [(tn, c)]
              | tn == escrowValidatorAsTokenName -> c
              | otherwise -> 0
            _ -> 0

        validBid :: Bid -> Value -> Value -> Bool
        validBid Bid {..} expectedValue utxoValue =
          let
            bidIsForTheRightAuction = aValue `geq` expectedValue
            bidHasEnoughAda = lovelaces utxoValue >= bidAmount
            bidCountIsOne = bidTokenCount utxoValue == 1

          in TRACE_IF_FALSE("Some bid is for a different auction", bidIsForTheRightAuction)
          && TRACE_IF_FALSE("Bid does not have enough ada"       , bidHasEnoughAda)
          && TRACE_IF_FALSE("Missing bid token"                  , bidCountIsOne)

        -- Get the bids from the datum
        toReturn :: [Bid]
        mTheBid :: Maybe Bid
        allBids :: Bool
        bidCount :: Integer

        -- We need to combine multiple traversals into one to reduce the memory usage.
        (allBids, !bidCount, !mTheBid, toReturn) = case convertInputs' atxInfoInputs atxInfoData aEscrowValidator of
          [] -> TRACE_ERROR("Missing bid inputs")
          xs -> foldl
            (\(!oldB, !c, !mCurrentHighest, bs) (x, y) ->
              let
                currentBid = (convertEscrowInputToBid aDeadline x)
                newB = validBid theBid (bdValue (eliData x)) y && oldB
                (newTheBid, returns) = case mCurrentHighest of
                      Nothing -> (Just currentBid, bs)
                      Just !currentHighest
                        | bidAmount currentHighest > bidAmount currentBid
                          -> (Just currentHighest, currentBid:bs)
                        | otherwise -> (Just currentBid, currentHighest:bs)
              in  ( newB
                  , c+1
                  , newTheBid
                  , returns
                  )
            )
            (True, 0, aHighBid, [])
            xs

        theBid :: Bid
        theBid = case mTheBid of
          Just x -> x
          _ -> TRACE_ERROR("No bids")

        allBidTokensAreBurned :: Bool
        allBidTokensAreBurned
          = bidTokenCount atxInfoMint == negate bidCount

        allLowerBidsReturnedToOwners :: Bool
        allLowerBidsReturnedToOwners = all (bidIsPaid atxInfoOutputs) (M.toList (mergeBids toReturn))
        -- Ensure the amount is great than the current
        -- min bid, e.g. the reserve price or last bid.
        sufficientBid :: Integer -> Bool
        sufficientBid amount =
          amount >= case aHighBid of
            Nothing -> aMinBid
            Just Bid{..} -> bidAmount + 1

        ownOutput   :: Value
        outputDatum :: Auction

        -- Clean up with better utilities
        (outputDatum, ownOutput) = case getContinuingOutputs' atxInfoData thisValidator atxInfoOutputs of
          [(x, y)] -> (x, y)
          _ -> TRACE_ERROR("expected exactly one continuing output")

        -- Make sure we are setting the next datum correctly
        -- Everything should be the same, but we should
        -- update the latest bid.
        correctBidOutputDatum :: Bid -> Bool
        correctBidOutputDatum b
          = outputDatum == (auction { aHighBid = Just b })

        oldBidAmount :: Integer
        oldBidAmount = case aHighBid of
          Nothing -> 0
          Just Bid {..} -> bidAmount

        bidDiff :: Integer
        bidDiff = bidAmount theBid - oldBidAmount

        -- The new value on the script should be the aValue
        correctBidOutputValue :: Bool
        correctBidOutputValue =
          ownOutput `geq` (actualScriptValue <> A.lovelaceValueOf bidDiff)

      in TRACE_IF_FALSE("bid too low"                               , (sufficientBid $ bidAmount theBid))
      && TRACE_IF_FALSE("wrong output datum"                        , (correctBidOutputDatum theBid))
      && TRACE_IF_FALSE("wrong output value"                        , correctBidOutputValue)
      && TRACE_IF_FALSE("All bids but the highest were not returned", allLowerBidsReturnedToOwners)
      && TRACE_IF_FALSE("Not all bid tokens are burned"             , allBidTokensAreBurned)
      && allBids

    Close ->
      let
        -- Closing is allowed if the deadline is before than the valid tx
        -- range. The deadline is past.
        correctCloseSlotRange :: Bool
        correctCloseSlotRange = aBatcherDeadline `before` atxInfoValidRange

        -- Helper to make sure the pkh is paid at least the
        getsValue :: PubKeyHash -> Value -> Bool
        getsValue h v = valuePaidTo' atxInfoOutputs h `geq` v

      in TRACE_IF_FALSE_CLOSE("too early", correctCloseSlotRange)
      && case aHighBid of
          Nothing
            -> TRACE_IF_FALSE_CLOSE(
                "expected seller to get token",
                (getsValue aSeller aValue))
          Just Bid{..}
            -> TRACE_IF_FALSE_CLOSE(
                "expected highest bidder to get token",
                (getsValue bidBidder aValue))
            && TRACE_IF_FALSE_CLOSE(
                "expected all sellers to get highest bid",
                (payoutIsValid bidAmount atxInfoOutputs aPayoutPercentages))

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
auctionWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
auctionWrapped = wrap mkValidator

validator :: Scripts.Validator
validator = mkValidatorScript
  $$(PlutusTx.compile [|| auctionWrapped ||])

auctionScriptHash :: ValidatorHash
auctionScriptHash = validatorHash validator
-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
auctionScript :: PlutusScript PlutusScriptV1
auctionScript
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  $ validator
