module AOC2023.Day07
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digits, fromParser, spaces)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (maximumBy)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Text.Parsec (alphaNum, many, manyTill, newline, optional, parse)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

-- N.B. earlier in datatype means lower Ord value
data Card = Joker | N Int | T | J | Q | K | A
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
  deriving (Show)

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
    group = flattenMap . handleJokers . foldr (\c -> M.insertWith (+) c 1) M.empty
      where
        handleJokers :: M.Map Card Int -> M.Map Card Int
        handleJokers m
          | numJokers == 5 = m
          | otherwise = M.adjust (+ numJokers) mostCommon otherCards
          where
            mostCommon :: Card
            mostCommon =
              fst
                . maximumBy (\a b -> snd a `compare` snd b)
                $ M.toList otherCards

            otherCards :: M.Map Card Int = M.delete Joker m

            numJokers :: Int = fromMaybe 0 $ M.lookup Joker m

        flattenMap :: M.Map Card Int -> [[Card]]
        flattenMap =
          L.sortOn (Down . length)
            . map (\(c, num) -> replicate num c)
            . M.toList

card :: Bool -> Parser Card
card jokerRule = do
  c <- alphaNum
  pure $
    if isDigit c
      then N (digitToInt c)
      else
        if jokerRule && c == 'J'
          then Joker
          else (read [c] :: Card)

bid :: Bool -> Parser Bid
bid jokerRule = do
  cards <- manyTill (card jokerRule) (char ' ')
  spaces
  bidNum <- digits
  optional newline
  pure $ Bid (Hand (calcHandStrength cards) cards) bidNum

calcWinnings :: [Bid] -> Int
calcWinnings =
  sum
    . zipWith (\i (Bid _ num) -> i * num) [1 ..]
    . L.sortOn (\(Bid hand _) -> hand)

part1 :: Solution
part1 = fromParser calcWinnings . parse (many $ bid False) ""

part2 :: Solution
part2 = fromParser calcWinnings . parse (many $ bid True) ""
