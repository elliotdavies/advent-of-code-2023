{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day04
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digits, fromParser, spaces, spaceSeparatedDigits)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec (parse)
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser)

data Card = Card Int (S.Set Int) (S.Set Int)
  deriving (Show)

card :: Parser Card
card = do
  string "Card"
  spaces
  cardId <- digits
  string ":"
  spaces
  winningNums <- spaceSeparatedDigits
  string "|"
  spaces
  allNums <- spaceSeparatedDigits
  pure $ Card cardId (S.fromList winningNums) (S.fromList allNums)

numMatches :: Card -> Int
numMatches (Card _ winningNums allNums) = S.size (S.intersection allNums winningNums)

points :: Card -> Int
points c = let n = numMatches c in if n == 0 then 0 else 2 ^ (n - 1)

part1 :: Solution
part1 input =
  fromParser
    (sum . map points)
    $ mapM (parse card "") (lines input)

cardMapping :: [Card] -> M.Map Int Int
cardMapping = foldr (\(Card cardId _ _) -> M.insert cardId 1) M.empty

stepCards :: M.Map Int Int -> [Card] -> M.Map Int Int
stepCards = foldl go
  where
    go acc c@(Card cardId _ _) =
      foldr (M.adjust (+ incBy)) acc idsToInc
      where
        idsToInc = [(cardId + 1) .. (cardId + numMatches c)]
        incBy = acc M.! cardId

part2 :: Solution
part2 input =
  fromParser
    (\cs -> sum . M.elems $ stepCards (cardMapping cs) cs)
    $ mapM (parse card "") (lines input)
