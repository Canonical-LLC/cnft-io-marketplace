module Canonical.Auction
  ( script
  , Auction(..)
  , Action(..)
  , payoutPerAddress
  , mergeSort
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Canonical.Escrow
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import           PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import qualified PlutusTx.AssocMap as A
import Plutus.V1.Ledger.Credential
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#endif

type BidEscrowLockerInput = EscrowLockerInput Integer

extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums where
  go = \case
    [] -> TRACE_ERROR("Failed to find datum")
    (x, y):xs ->
      if x == dh then
        y
      else
        go xs

extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

convertInputs
  :: UnsafeFromData a
  => [TxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(a, Value)]
convertInputs ins datums vh = go [] ins  where
  go acc = \case
    [] -> acc
    TxInInfo
      {txInInfoResolved = TxOut
        { txOutDatumHash = Just dh
        , txOutAddress = Address {..}
        , txOutValue
        }
      }:xs ->

        if ScriptCredential vh == addressCredential then

          go  ( ( unsafeFromBuiltinData (extractDatumBytes datums dh)
                , txOutValue
                )
              : acc
              )
              xs
        else
          go acc xs
    _ -> TRACE_ERROR("Script input missing datum hash")

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Bid = Bid
  { bidBidder :: !PubKeyHash
  , bidAmount :: !Integer
  }

data Auction = Auction
  { aSeller            :: !PubKeyHash
  , aStartTime         :: !POSIXTime
  , aDeadline          :: !POSIXTime
  , aMinBid            :: !Integer
  , aCurrency          :: !CurrencySymbol
  , aToken             :: !TokenName
  , aPayoutPercentages :: !(A.Map PubKeyHash Integer)
  , aHighBid           :: !(Maybe Bid)
  , aEscrowValidator   :: ValidatorHash
  }

data Action = CollectBids | Close

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

instance Eq Auction where
  {-# INLINABLE (==) #-}
  x == y
    =  (aSeller            x == aSeller            y)
    && (aStartTime         x == aStartTime         y)
    && (aDeadline          x == aDeadline          y)
    && (aMinBid            x == aMinBid            y)
    && (aCurrency          x == aCurrency          y)
    && (aToken             x == aToken             y)
    && (aPayoutPercentages x == aPayoutPercentages y)
    && (aHighBid           x == aHighBid           y)
    && (aEscrowValidator   x == aEscrowValidator   y)

instance Eq Bid where
  {-# INLINABLE (==) #-}
  x == y
    =  (bidBidder x == bidBidder y)
    && (bidAmount x == bidAmount y)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

-------------------------------------------------------------------------------
-- Sorting Utilities
-------------------------------------------------------------------------------
{-# INLINABLE drop #-}
drop :: Integer -> [a] -> [a]
drop n l@(_:xs) =
    if n <= 0 then l
    else drop (n-1) xs
drop _ [] = []

{-# INLINABLE merge #-}
merge :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
merge as@(a:as') bs@(b:bs') =
    if snd a <= snd b
    then a:(merge as'  bs)
    else b:(merge as bs')
merge [] bs = bs
merge as [] = as

{-# INLINABLE mergeSort #-}
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

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Lovelaces
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE lovelacesPaidTo #-}
lovelacesPaidTo :: TxInfo -> PubKeyHash -> Integer
lovelacesPaidTo info pkh = lovelaces (valuePaidTo info pkh)

{-# INLINABLE minAda #-}
minAda :: Lovelaces
minAda = 1_000_000

{-# INLINABLE sortPercents #-}
sortPercents :: A.Map PubKeyHash Percent -> [(PubKeyHash, Percent)]
sortPercents = mergeSort . A.toList

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
{-# INLINABLE payoutPerAddress #-}
payoutPerAddress :: Integer -> [(PubKeyHash, Percent)] -> [(PubKeyHash, Lovelaces)]
payoutPerAddress total percents = go total 1000 percents where
  go left totalPercent = \case
    [] -> traceError "No seller percentage specified"
    [(pkh, _)] -> [(pkh, left)]
    (pkh, percent) : rest ->
      let !percentOfPot = applyPercent totalPercent left percent
          !percentOf = max minAda percentOfPot
      in (pkh, percentOf) : go (left - percentOf) (totalPercent - percent) rest

{-# INLINABLE applyPercent #-}
applyPercent :: Integer -> Lovelaces -> Percent -> Lovelaces
applyPercent divider inVal pct = (inVal * pct) `divide` divider

-- Sort the payout map from least to greatest.
-- Compute the payouts for each address.
-- Check that each address has received their payout.
{-# INLINABLE payoutIsValid #-}
payoutIsValid :: Lovelaces -> TxInfo -> A.Map PubKeyHash Percent -> Bool
payoutIsValid total info
  = all (paidapplyPercent info)
  . payoutPerAddress total
  . sortPercents

-- For a given address and percentage pair, verify
-- they received greater or equal to their percentage
-- of the input.
{-# INLINABLE paidapplyPercent #-}
paidapplyPercent :: TxInfo -> (PubKeyHash, Lovelaces) -> Bool
paidapplyPercent info (addr, owed)
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
{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
{-# INLINABLE getOnlyScriptInput #-}
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"

  in txOutValue . txInInfoResolved $ input

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
mergeBids :: [Bid] -> Map PubKeyHash Integer
mergeBids
  = foldr
      (\Bid {..} acc
        -> M.unionWith (+) (M.singleton bidBidder bidAmount) acc
      )
      M.empty

bidIsPaid :: TxInfo -> (PubKeyHash, Integer) -> Bool
bidIsPaid info (bid, amount)
  = Value.valueOf (valuePaidTo info bid) Ada.adaSymbol Ada.adaToken
  >= amount

partitionBids :: [Bid] -> (Bid, [Bid])
partitionBids (x:xs) = go x [] xs where
  go highest prev = \case
    [] -> (highest, prev)
    y:ys
      | bidAmount y > bidAmount highest -> go y (highest:prev) ys
      | otherwise   -> go highest (y:prev) ys
partitionBids _ = TRACE_ERROR("expected non-empty bids")

convertEscrowInputToBid :: BidEscrowLockerInput -> Bid
convertEscrowInputToBid EscrowLockerInput {..} = Bid
  { bidBidder = eliOwner
  , bidAmount = eliData
  }

bidHasEnoughAda :: (Bid, Value) -> Bool
bidHasEnoughAda (Bid{..}, v)
  =  Value.valueOf v Ada.adaSymbol Ada.adaToken
  >= bidAmount
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
{-# INLINABLE mkValidator #-}
mkValidator :: Auction -> Action -> ScriptContext -> Bool
mkValidator auction@Auction {..} action ctx =
  let
    info :: TxInfo
    info@TxInfo {..} = scriptContextTxInfo ctx

    -- Helper to make sure the pkh is paid at least the value.
    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v = valuePaidTo info h `Value.geq` v

    -- The asset we are auctioning as a Value
    tokenValue :: Value
    tokenValue = Value.singleton aCurrency aToken 1

    -- The value we expect on the script input based on
    -- datum.
    expectedScriptValue :: Value
    expectedScriptValue = case aHighBid of
      Nothing -> tokenValue
      Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf bidAmount

    actualScriptValue :: Value
    actualScriptValue = getOnlyScriptInput info
    -- Ensure the value is on the script address and there is
    -- only one script input.
    correctInputValue :: Bool
    correctInputValue = actualScriptValue `Value.geq` expectedScriptValue

  -- Always perform the input check
  in traceIfFalse "wrong input value" correctInputValue
  && case action of
    CollectBids ->
      let
        -- Get the bids from the datum
        escrowBidsAndValues :: [(Bid, Value)]
        escrowBidsAndValues = case convertInputs txInfoInputs txInfoData aEscrowValidator of
          [] -> TRACE_ERROR("Missing bid inputs")
          xs -> map (\(x, y) -> (convertEscrowInputToBid x, y)) xs

        ensureBidsHaveEnoughAda :: Bool
        ensureBidsHaveEnoughAda = all bidHasEnoughAda escrowBidsAndValues

        escrowBids :: [Bid]
        escrowBids = map fst escrowBidsAndValues

        currentHighestBidder :: [Bid]
        currentHighestBidder = case aHighBid of
          Nothing -> []
          Just x  -> [x]

        (theBid, toReturn) = partitionBids (currentHighestBidder <> escrowBids)

        -- | Read the eliOwner field and ensure
        --
        allLowerBidsReturnedToOwners :: Bool
        allLowerBidsReturnedToOwners = all (bidIsPaid info) (M.toList (mergeBids toReturn))
        -- Ensure the amount is great than the current
        -- min bid, e.g. the reserve price or last bid.
        sufficientBid :: Integer -> Bool
        sufficientBid amount =
          amount >= case aHighBid of
            Nothing -> aMinBid
            Just Bid{..} -> bidAmount + 1

        ownOutput   :: TxOut
        outputDatum :: Auction

        -- Clean up with better utilities
        (ownOutput, outputDatum) = case getContinuingOutputs ctx of
          [o] -> case txOutDatumHash o of
            Nothing -> traceError "wrong output type"
            Just h -> case findDatum h info of
              Nothing -> traceError "datum not found"
              Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                Just ad' -> (o, ad')
                Nothing  -> traceError "error decoding data"
          _ -> traceError "expected exactly one continuing output"

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

        -- The new value on the script should be the tokenValue
        correctBidOutputValue :: Bool
        correctBidOutputValue =
          txOutValue ownOutput `Value.geq` (actualScriptValue <> Ada.lovelaceValueOf bidDiff)

        -- Bidding is allowed if the start time is before the tx interval
        -- deadline is later than the valid tx
        -- range. The deadline is in the future.
        correctBidSlotRange :: Bool
        correctBidSlotRange
          =  aStartTime `before` txInfoValidRange
          && aDeadline `after` txInfoValidRange

      in traceIfFalse "bid too low"        (sufficientBid $ bidAmount theBid)
      && traceIfFalse "wrong output datum" (correctBidOutputDatum theBid)
      && traceIfFalse "wrong output value" correctBidOutputValue
      && traceIfFalse "too late"           correctBidSlotRange
      && traceIfFalse "All bids but the highest were not returned" allLowerBidsReturnedToOwners
      && traceIfFalse "Some bids do not have enough ada" ensureBidsHaveEnoughAda

    Close ->
      let
        -- Closing is allowed if the deadline is before than the valid tx
        -- range. The deadline is past.
        correctCloseSlotRange :: Bool
        correctCloseSlotRange = aDeadline `before` txInfoValidRange

      in traceIfFalse "too early" correctCloseSlotRange
      && case aHighBid of
          Nothing
            -> traceIfFalse
                "expected seller to get token"
                (getsValue aSeller tokenValue)
          Just Bid{..}
            -> traceIfFalse
                "expected highest bidder to get token"
                (getsValue bidBidder tokenValue)
            && traceIfFalse
                "expected all sellers to get highest bid"
                (payoutIsValid bidAmount info aPayoutPercentages)

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
data Auctioning
instance Scripts.ValidatorTypes Auctioning where
  type instance DatumType Auctioning = Auction
  type instance RedeemerType Auctioning = Action

typedValidator :: Scripts.TypedValidator Auctioning
typedValidator =
  Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
script :: PlutusScript PlutusScriptV1
script
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  $ validator
