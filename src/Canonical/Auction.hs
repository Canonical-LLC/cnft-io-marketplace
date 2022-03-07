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
import qualified Ledger.Ada as Ada
import qualified PlutusTx.AssocMap as A
import           Ledger.Value
import           Plutus.V1.Ledger.Credential
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Canonical.Shared
import           Canonical.BidMinter

#define DEBUG

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#endif

type BidEscrowLockerInput = EscrowLockerInput BidData

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


data Auction = Auction
  { aSeller            :: PubKeyHash
  , aStartTime         :: Maybe POSIXTime
  , aDeadline          :: POSIXTime
  , aBatcherDeadline   :: POSIXTime
  , aMinBid            :: Integer
  , aPayoutPercentages :: (A.Map PubKeyHash Integer)
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
    && (aStartTime         x == aStartTime         y)
    && (aDeadline          x == aDeadline          y)
    && (aBatcherDeadline   x == aBatcherDeadline   y)
    && (aMinBid            x == aMinBid            y)
    && (aPayoutPercentages x == aPayoutPercentages y)
    && (aHighBid           x == aHighBid           y)
    && (aEscrowValidator   x == aEscrowValidator   y)
    && (aValue             x == aValue             y)
    && (aBidMinterPolicyId x == aBidMinterPolicyId y)


PlutusTx.unstableMakeIsData ''Auction
PlutusTx.unstableMakeIsData ''Action


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

isScriptCredential :: Credential -> Bool
isScriptCredential = \case
  ScriptCredential _ -> True
  _ -> False

expectedScripts :: TxInfo -> ValidatorHash -> ValidatorHash -> Bool
expectedScripts TxInfo {..} auctionValidator escrowValidator =
  let
    auctionCredential :: Credential
    auctionCredential = ScriptCredential auctionValidator

    escrowCredential :: Credential
    escrowCredential = ScriptCredential escrowValidator

    inputCredentials :: [Credential]
    inputCredentials =
      filter isScriptCredential
        (map (addressCredential . txOutAddress . txInInfoResolved) txInfoInputs)

    onlyAuctionOrEscrow :: Bool
    onlyAuctionOrEscrow =
      all (\x -> auctionCredential == x || escrowCredential == x) inputCredentials

    onlyOneAuctionScript :: Bool
    onlyOneAuctionScript =
      length (filter (== auctionCredential) inputCredentials) == 1

  in TRACE_IF_FALSE("Has own of this auctionValidator", onlyOneAuctionScript)
  && TRACE_IF_FALSE("Invalid script inputs", onlyAuctionOrEscrow)

-- Verify that there is only one script input and get it's Value.
getScriptValue :: TxInfo -> ValidatorHash -> Value
getScriptValue TxInfo {..} theValidator =
  let
    theCredential :: Credential
    theCredential = ScriptCredential theValidator

    isScriptInput :: TxInInfo -> Bool
    isScriptInput = (theCredential ==) . addressCredential . txOutAddress . txInInfoResolved

    input = case filter isScriptInput txInfoInputs of
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
bidIsPaid info (theUser, amount)
  = valueOf (valuePaidTo info theUser) Ada.adaSymbol Ada.adaToken
  >= amount

partitionBids :: [Bid] -> (Bid, [Bid])
partitionBids (x:xs) = go x [] xs where
  go highest prev = \case
    [] -> (highest, prev)
    y:ys
      | bidAmount y > bidAmount highest -> go y (highest:prev) ys
      | otherwise   -> go highest (y:prev) ys
partitionBids _ = TRACE_ERROR("expected non-empty bids")

convertEscrowInputToBid :: Maybe POSIXTime -> POSIXTime -> BidEscrowLockerInput -> Bid
convertEscrowInputToBid startTime deadline EscrowLockerInput {..} =
  if fromMaybe 0 startTime `before` bdBidValidRange eliData &&
    deadline `after` bdBidValidRange eliData then
    Bid
      { bidBidder = eliOwner
      , bidAmount = bdBid eliData
      }
  else
    TRACE_ERROR("bid expired")

bidHasEnoughAda :: (Bid, Value, Value) -> Bool
bidHasEnoughAda (Bid{..}, _, v)
  =  valueOf v Ada.adaSymbol Ada.adaToken
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

    -- Helper to make sure the pkh is paid at least the
    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v = valuePaidTo info h `geq` v

    -- The value we expect on the script input based on
    -- datum.
    expectedScriptValue :: Value
    expectedScriptValue = case aHighBid of
      Nothing -> aValue
      Just Bid{..} -> aValue <> Ada.lovelaceValueOf bidAmount

    thisValidator :: ValidatorHash
    thisValidator = ownHash ctx

    hasValidatorScripts :: Bool
    hasValidatorScripts = expectedScripts info thisValidator aEscrowValidator

    actualScriptValue :: Value
    actualScriptValue = getScriptValue info thisValidator
    -- Ensure the value is on the script address and there is
    -- only one script input.
    correctInputValue :: Bool
    correctInputValue = actualScriptValue `geq` expectedScriptValue

  -- Always perform the input check
  in traceIfFalse "wrong input value" correctInputValue
  && case action of
    CollectBids ->
      let
        -- Get the bids from the datum
        escrowBidsAndValues :: [(Bid, Value, Value)]
        escrowBidsAndValues = case convertInputs txInfoInputs txInfoData aEscrowValidator of
          [] -> TRACE_ERROR("Missing bid inputs")
          xs -> map (\(x, y) -> (convertEscrowInputToBid aStartTime aDeadline x, bdValue (eliData x), y)) xs

        hasBidToken :: Bool
        hasBidToken =
          all (\(_, _, Value v) -> case M.lookup aBidMinterPolicyId v of
                  Nothing -> False
                  Just m -> case M.toList m of
                    [(TokenName tn, c)]
                      | c == 1 -> ValidatorHash tn == aEscrowValidator
                      | otherwise -> False
                    _ -> False
              )
              escrowBidsAndValues

        bidsAreForTheRightAuction :: Bool
        bidsAreForTheRightAuction =
          all (`geq` aValue) (map (\(_, v, _) -> v) escrowBidsAndValues)

        ensureBidsHaveEnoughAda :: Bool
        ensureBidsHaveEnoughAda = all bidHasEnoughAda escrowBidsAndValues

        escrowBids :: [Bid]
        escrowBids = map (\(x, _, _) -> x) escrowBidsAndValues

        currentHighestBidder :: [Bid]
        currentHighestBidder = case aHighBid of
          Nothing -> []
          Just x  -> [x]

        -- We need to return the expired bids
        (theBid, toReturn) = partitionBids (currentHighestBidder <> escrowBids)

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

        -- The new value on the script should be the aValue
        correctBidOutputValue :: Bool
        correctBidOutputValue =
          txOutValue ownOutput `geq` (actualScriptValue <> Ada.lovelaceValueOf bidDiff)

      in traceIfFalse "bid too low"        (sufficientBid $ bidAmount theBid)
      && traceIfFalse "wrong output datum" (correctBidOutputDatum theBid)
      && traceIfFalse "wrong output value" correctBidOutputValue
      && traceIfFalse "All bids but the highest were not returned" allLowerBidsReturnedToOwners
      && traceIfFalse "Some bids do not have enough ada" ensureBidsHaveEnoughAda
      && traceIfFalse "Some bids are for a different auction" bidsAreForTheRightAuction
      && traceIfFalse "Has incorrect scripts" hasValidatorScripts
      && traceIfFalse "Missing bid token" hasBidToken

    Close ->
      let
        -- Closing is allowed if the deadline is before than the valid tx
        -- range. The deadline is past.
        correctCloseSlotRange :: Bool
        correctCloseSlotRange = aBatcherDeadline `before` txInfoValidRange

      in traceIfFalse "too early" correctCloseSlotRange
      && case aHighBid of
          Nothing
            -> traceIfFalse
                "expected seller to get token"
                (getsValue aSeller aValue)
          Just Bid{..}
            -> traceIfFalse
                "expected highest bidder to get token"
                (getsValue bidBidder aValue)
            && traceIfFalse
                "expected all sellers to get highest bid"
                (payoutIsValid bidAmount info aPayoutPercentages)

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
