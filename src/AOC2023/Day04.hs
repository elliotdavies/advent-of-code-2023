{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day04
  ( Input,
    Output,
    part1,
    part2,
  )
where

import AOC2023.Lib (digits)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace (traceShow, traceShowId)
import Text.Parsec (many, parse)
import Text.Parsec.Char (spaces, string)
import Text.Parsec.String (Parser)

type Input = String

type Output = Int

data Card = Card Int (S.Set Int) (S.Set Int)
  deriving (Show)

spaceSeparatedDigits :: Parser [Int]
spaceSeparatedDigits = many $ do
  ds <- digits
  spaces
  pure ds

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

part1 :: Input -> Output
part1 input =
  let results = sequence $ parse card "" <$> lines input
   in case results of
        Right cs -> sum $ map points cs
        Left err -> length $ traceShowId $ show err

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

part2 :: Input -> Output
part2 input =
  let results = sequence $ parse card "" <$> lines input
   in case results of
        Right cs -> sum $ M.elems $ stepCards (cardMapping cs) cs
        Left err -> length $ traceShowId $ show err
