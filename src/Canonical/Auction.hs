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
#include "DebugUtilities.h"

type BidEscrowLockerInput = EscrowLockerInput BidData

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
    let flt AuctionTxOut{ atxOutAddress = AuctionAddress (PubKeyCredential pk') _, atxOutValue } | pk == pk' = Just atxOutValue
                                                                                     | otherwise = Nothing
        flt _                     = Nothing
    in mapMaybe flt outs

getContinuingOutputs'
  :: DataConstraint(a)
  => [(DatumHash, Datum)]
  -> ValidatorHash
  -> [AuctionTxOut]
  -> [(a, AuctionTxOut)]
getContinuingOutputs' datums vh outs =
  map
    (\txout@AuctionTxOut {..} -> case atxOutDatumHash of
          Just dh -> (extractData datums dh, txout)
          Nothing -> TRACE_ERROR("Missing Datum Hash")
    )
    (filter
      (\AuctionTxOut {..} -> aaddressCredential atxOutAddress
        == ScriptCredential vh)
      outs
    )

convertInputs'
  :: UnsafeFromData a
  => [AuctionTxInInfo]
  -> [(DatumHash, Datum)]
  -> ValidatorHash
  -> [(a, Value)]
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
-- Types
-------------------------------------------------------------------------------
data Auction = Auction
  { aSeller            :: PubKeyHash
  , aDeadline          :: POSIXTime
  , aBatcherDeadline   :: POSIXTime
  , aMinBid            :: Integer
  , aPayoutPercentages :: (A.Map PubKeyHash Integer)
  , aHighBid           :: (Maybe Bid)
  , aEscrowValidator   :: ValidatorHash
  , aValue             :: Value
  , aBidMinterPolicyId :: CurrencySymbol
  , aBoostTokenName    :: TokenName
  , aBoostPolicyId     :: CurrencySymbol
  , aBoostPayoutPkh    :: PubKeyHash
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
    && (aBoostTokenName    x == aBoostTokenName    y)
    && (aBoostPolicyId     x == aBoostPolicyId     y)
    && (aBoostPayoutPkh    x == aBoostPayoutPkh    y)

unstableMakeIsData ''Auction
unstableMakeIsData ''Action
makeIsDataIndexed  ''AuctionScriptPurpose [('ASpending,1)]
unstableMakeIsData ''AuctionTxInfo
unstableMakeIsData ''AuctionScriptContext
unstableMakeIsData ''AuctionAddress
unstableMakeIsData ''AuctionTxOut
unstableMakeIsData ''AuctionTxInInfo

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

lovelaces :: Value -> Lovelaces
lovelaces = Ada.getLovelace . Ada.fromValue

lovelacesPaidTo :: [AuctionTxOut] -> PubKeyHash -> Integer
lovelacesPaidTo info pkh = lovelaces (valuePaidTo' info pkh)

minAda :: Lovelaces
minAda = 1_000_000

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
payoutIsValid :: Lovelaces -> [AuctionTxOut] -> A.Map PubKeyHash Percent -> Bool
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

expectedScripts :: [AuctionTxInInfo] -> ValidatorHash -> ValidatorHash -> Bool
expectedScripts theInputs auctionValidator escrowValidator =
  let
    auctionCredential :: Credential
    auctionCredential = ScriptCredential auctionValidator

    escrowCredential :: Credential
    escrowCredential = ScriptCredential escrowValidator

    inputCredentials :: [Credential]
    inputCredentials =
      filter isScriptCredential
        (map (aaddressCredential . atxOutAddress . atxInInfoResolved) theInputs)

    onlyAuctionOrEscrow :: Bool
    onlyAuctionOrEscrow =
      all (\x -> auctionCredential == x || escrowCredential == x) inputCredentials

    onlyOneAuctionScript :: Bool
    onlyOneAuctionScript =
      length (filter (== auctionCredential) inputCredentials) == 1

  in TRACE_IF_FALSE("Has own of this auctionValidator", onlyOneAuctionScript)
  && TRACE_IF_FALSE("Invalid script inputs", onlyAuctionOrEscrow)

-- Verify that there is only one script input and get it's Value.
getScriptValue :: [AuctionTxInInfo] -> ValidatorHash -> Value
getScriptValue theInputs theValidator =
  let
    theCredential :: Credential
    theCredential = ScriptCredential theValidator

    isScriptInput :: AuctionTxInInfo -> Bool
    isScriptInput = (theCredential ==) . aaddressCredential . atxOutAddress . atxInInfoResolved

    input = case filter isScriptInput theInputs of
      [i] -> i
      _ -> TRACE_ERROR("expected exactly one script input")

  in atxOutValue . atxInInfoResolved $ input

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

bidIsPaid :: [AuctionTxOut] -> (PubKeyHash, Integer) -> Bool
bidIsPaid theOutputs (theUser, amount)
  = valueOf (valuePaidTo' theOutputs theUser) Ada.adaSymbol Ada.adaToken
  >= amount

partitionBids :: [Bid] -> (Bid, [Bid])
partitionBids (x:xs) = go x [] xs where
  go highest prev = \case
    [] -> (highest, prev)
    y:ys
      | bidAmount y > bidAmount highest -> go y (highest:prev) ys
      | bidAmount y == bidAmount highest &&
        bidTime y < bidTime highest -> go y (highest:prev) ys
      | otherwise   -> go highest (y:prev) ys
partitionBids _ = TRACE_ERROR("expected non-empty bids")

convertEscrowInputToBid :: POSIXTime -> BidEscrowLockerInput -> Bid
convertEscrowInputToBid deadline EscrowLockerInput {..} =
  if bdValidEndTime eliData < deadline then
    Bid
      { bidBidder = eliOwner
      , bidAmount = bdBid eliData
      , bidTime   = bdValidEndTime eliData
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
mkValidator :: Auction -> Action -> AuctionScriptContext -> Bool
mkValidator auction@Auction {..} action AuctionScriptContext
  { aScriptContextTxInfo = AuctionTxInfo {..}
  , aScriptContextPurpose = ASpending thisOutRef
  } =
  let
    -- Helper to make sure the pkh is paid at least the
    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v = valuePaidTo' atxInfoOutputs h `geq` v

    -- The value we expect on the script input based on
    -- datum.
    expectedScriptValue :: Value
    expectedScriptValue = case aHighBid of
      Nothing -> aValue
      Just Bid{..} -> aValue <> Ada.lovelaceValueOf bidAmount

    thisValidator :: ValidatorHash
    thisValidator = ownHash' atxInfoInputs thisOutRef

    hasValidatorScripts :: Bool
    hasValidatorScripts = expectedScripts atxInfoInputs thisValidator aEscrowValidator

    actualScriptValue :: Value
    actualScriptValue = getScriptValue atxInfoInputs thisValidator
    -- Ensure the value is on the script address and there is
    -- only one script input.
    correctInputValue :: Bool
    correctInputValue = actualScriptValue `geq` expectedScriptValue

  -- Always perform the input check
  in TRACE_IF_FALSE("wrong input value", correctInputValue)
  && case action of
    CollectBids ->
      let
        -- Get the bids from the datum
        escrowBidsAndValues :: [(Bid, Value, Value)]
        escrowBidsAndValues = case convertInputs' atxInfoInputs atxInfoData aEscrowValidator of
          [] -> TRACE_ERROR("Missing bid inputs")
          xs -> map (\(x, y) -> (convertEscrowInputToBid aDeadline x, bdValue (eliData x), y)) xs

        bidTokenCount :: Integer
        bidTokenCount =
          case M.lookup aBidMinterPolicyId (getValue atxInfoMint) of
            Nothing -> 0
            Just m -> case M.toList m of
              [(TokenName tn, c)]
                | ValidatorHash tn == aEscrowValidator -> c
                | otherwise -> 0
              _ -> 0

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
        allLowerBidsReturnedToOwners = all (bidIsPaid atxInfoOutputs) (M.toList (mergeBids toReturn))
        -- Ensure the amount is great than the current
        -- min bid, e.g. the reserve price or last bid.
        sufficientBid :: Integer -> Bool
        sufficientBid amount =
          amount >= case aHighBid of
            Nothing -> aMinBid
            Just Bid{..} -> bidAmount + 1

        ownOutput   :: AuctionTxOut
        outputDatum :: Auction

        -- Clean up with better utilities
        (ownOutput, outputDatum) = case getContinuingOutputs' atxInfoData thisValidator atxInfoOutputs of
          [(x, y)] -> (y, x)
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
          atxOutValue ownOutput `geq` (actualScriptValue <> Ada.lovelaceValueOf bidDiff)

        allTokensBurned :: Bool
        allTokensBurned = negate (length escrowBids) == bidTokenCount

      in TRACE_IF_FALSE("bid too low"                               , (sufficientBid $ bidAmount theBid))
      && TRACE_IF_FALSE("wrong output datum"                        , (correctBidOutputDatum theBid))
      && TRACE_IF_FALSE("wrong output value"                        , correctBidOutputValue)
      && TRACE_IF_FALSE("All bids but the highest were not returned", allLowerBidsReturnedToOwners)
      && TRACE_IF_FALSE("Some bids do not have enough ada"          , ensureBidsHaveEnoughAda)
      && TRACE_IF_FALSE("Some bids are for a different auction"     , bidsAreForTheRightAuction)
      && TRACE_IF_FALSE("Has incorrect scripts"                     , hasValidatorScripts)
      && TRACE_IF_FALSE("Missing bid token"                         , hasBidToken)
      && TRACE_IF_FALSE("Not all bid tokens burned"                 , allTokensBurned)

    Close ->
      let
        -- Closing is allowed if the deadline is before than the valid tx
        -- range. The deadline is past.
        correctCloseSlotRange :: Bool
        correctCloseSlotRange = aBatcherDeadline `before` atxInfoValidRange

      in TRACE_IF_FALSE_CLOSE("too early", correctCloseSlotRange)
      && case aHighBid of
          Nothing
            -> TRACE_IF_FALSE_CLOSE(
                "expected seller to get token",
                (getsValue aSeller aValue))
          Just Bid{..} ->
            let
              boostOf :: Value -> Integer
              boostOf v = valueOf v aBoostPolicyId aBoostTokenName

              activityTokenAmount :: Integer
              activityTokenAmount
                = max 1 $ bidAmount `divide` 20_000_000

              sellerPaidValue :: Value
              sellerPaidValue = valuePaidTo' atxInfoOutputs aSeller

              sellerGetsActivityToken :: Bool
              sellerGetsActivityToken
                = boostOf sellerPaidValue >= activityTokenAmount

              bidderValue :: Value
              bidderValue = valuePaidTo' atxInfoOutputs bidBidder

              buyerGetsActivityToken :: Bool
              buyerGetsActivityToken
                = boostOf bidderValue >= activityTokenAmount

              boostWentToMarketplace :: Bool
              boostWentToMarketplace = boostOf actualScriptValue <= boostOf (valuePaidTo' atxInfoOutputs aBoostPayoutPkh)

            in TRACE_IF_FALSE_CLOSE(
                "expected highest bidder to get token",
                (bidderValue `geq` aValue))
            && TRACE_IF_FALSE_CLOSE(
                "expected all sellers to get highest bid",
                (payoutIsValid bidAmount atxInfoOutputs aPayoutPercentages))
            && TRACE_IF_FALSE_CLOSE(
                "Seller did not get activity token",
                sellerGetsActivityToken)
            && TRACE_IF_FALSE_CLOSE(
                "Buyer did not get activity token",
                buyerGetsActivityToken)
            && TRACE_IF_FALSE_CLOSE(
                "Boost went to marketplace",
                boostWentToMarketplace)

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
auctionWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
auctionWrapped = wrap mkValidator

validator :: Scripts.Validator
validator = mkValidatorScript $
  $$(PlutusTx.compile [|| \c -> auctionWrapped c ||])

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
  $ serialise
    validator
