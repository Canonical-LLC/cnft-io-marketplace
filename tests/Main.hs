{-# LANGUAGE NumericUnderscores #-}
import Test.Hspec
import Prelude
import Canonical.Auction
import qualified Data.List as L
import           Ledger
import Data.Foldable

main :: IO ()
main = hspec spec

makeSplits :: [[(PubKeyHash, Integer)]]
makeSplits = makeSplits2ways <> makeSplits3ways <> makeSplits4ways

makeSplits2ways :: [[(PubKeyHash, Integer)]]
makeSplits2ways = do
  x <- [0 .. 500]
  let y = 1000 - x
  pure
    [ (PubKeyHash "a", x)
    , (PubKeyHash "b", y)
    ]

makeSplits3ways :: [[(PubKeyHash, Integer)]]
makeSplits3ways = do
  x <- [0 .. 333]
  y <- [x .. (1000 - x) `div` 2]
  let z = 1000 - (x + y)
  pure
    [ (PubKeyHash "a", x)
    , (PubKeyHash "b", y)
    , (PubKeyHash "c", z)
    ]

makeSplits4ways :: [[(PubKeyHash, Integer)]]
makeSplits4ways = do
  x <- [0 .. 250]
  y <- [x .. (1000 - x) `div` 3]
  z <- [y .. (1000 - (y + x)) `div` 2]
  let w = 1000 - (x + y + z)
  pure
    [ (PubKeyHash "a", x)
    , (PubKeyHash "b", y)
    , (PubKeyHash "c", z)
    , (PubKeyHash "d", w)
    ]

spec :: Spec
spec = do
  describe "payoutPerAddress" $ do
    it "mergeSort sorts least to greatest" $ do
      let
        initial =
          [ (PubKeyHash "a", 950)
          , (PubKeyHash "b", 30)
          , (PubKeyHash "c", 20)
          ]

        expected =
          [ (PubKeyHash "c", 20)
          , (PubKeyHash "b", 30)
          , (PubKeyHash "a", 950)
          ]

      mergeSort initial `shouldBe` expected

    it "Handles two minimums correctly" $ do
      let
        expected =
          [ (PubKeyHash "a", 1_000_000)
          , (PubKeyHash "b", 1_000_000)
          , (PubKeyHash "c", 8_000_000)
          ]

        initial =
          [ (PubKeyHash "a", 25)
          , (PubKeyHash "b", 25)
          , (PubKeyHash "c", 950)
          ]

      L.sort (payoutPerAddress 10_000_000 initial) `shouldBe` L.sort expected

    it "Handles one minimum correctly" $ do
      let
        expected =
          [ (PubKeyHash "a", 1_000_000)
          , (PubKeyHash "b", 1_421_052)
          , (PubKeyHash "c", 7_578_948)
          ]

        initial =
          [ (PubKeyHash "a", 50)
          , (PubKeyHash "b", 150)
          , (PubKeyHash "c", 800)
          ]

      L.sort (payoutPerAddress 10_000_000 initial) `shouldBe` L.sort expected

    it "Always payouts the total" $ do
      let splits = makeSplits

      forM_ splits $ \split -> do
        let payouts = payoutPerAddress 10_000_000 split

        sum (map snd payouts) `shouldBe` 10_000_000
