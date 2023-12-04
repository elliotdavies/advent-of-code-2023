{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day04
  ( Input,
    Output,
    part1,
    part2,
  )
where

import AOC2023.Lib (digits)
import qualified Data.Set as S
import Debug.Trace (traceShowId)
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

points :: Card -> Int
points (Card _ winningNums allNums) =
  let numWinners = S.size (S.intersection allNums winningNums)
   in if numWinners == 0 then 0 else 2 ^ (numWinners - 1)

part1 :: Input -> Output
part1 input =
  let results = sequence $ parse card "" <$> lines input
   in case results of
        Right cs -> sum $ map points cs
        Left err -> length $ traceShowId $ show err

part2 :: Input -> Output
part2 _ = 0
