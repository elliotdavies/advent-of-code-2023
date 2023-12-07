module AOC2023.Day07
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digits, fromParser, spaces)
import Data.Char (digitToInt, isAlpha)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import Text.Parsec (alphaNum, many, manyTill, newline, optional, parse)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

-- N.B. earlier in datatype means lower Ord value
data Card = N Int | T | J | Q | K | A
  deriving (Show, Read, Eq, Ord)

data HandStrength
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

data Hand = Hand HandStrength [Card]
  deriving (Show, Eq, Ord)

data Bid = Bid Hand Int

calcHandStrength :: [Card] -> HandStrength
calcHandStrength cards =
  case group cards of
    [[_, _, _, _, _]] -> FiveOfKind
    [[_, _, _, _], [_]] -> FourOfKind
    [[_, _, _], [_, _]] -> FullHouse
    [[_, _, _], [_], [_]] -> ThreeOfKind
    [[_, _], [_, _], [_]] -> TwoPair
    [[_, _], [_], [_], [_]] -> OnePair
    _ -> HighCard
  where
    group :: [Card] -> [[Card]]
    group = flattenMap . foldr (\c -> M.insertWith (+) c 1) M.empty
      where
        flattenMap :: M.Map Card Int -> [[Card]]
        flattenMap =
          L.sortOn (Down . length)
            . map (\(c, num) -> replicate num c)
            . M.toList

card :: Parser Card
card = do
  c <- alphaNum
  pure $ if isAlpha c then (read [c] :: Card) else N (digitToInt c)

bid :: Parser Bid
bid = do
  cards <- manyTill card (char ' ')
  spaces
  bidNum <- digits
  optional newline
  pure $ Bid (Hand (calcHandStrength cards) cards) bidNum

part1 :: Solution
part1 = fromParser go . parse (many bid) ""
  where
    go :: [Bid] -> Int
    go =
      sum
        . zipWith (\i (Bid _ num) -> i * num) [1 ..]
        . L.sortOn (\(Bid hand _) -> hand)

part2 :: Solution
part2 = undefined
