{-# LANGUAGE LambdaCase #-}

module AOC2023.Day03
  ( Input,
    Output,
    part1,
    part2,
  )
where

import AOC2023.Lib (digitsToInt, (!?!?))
import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

type Input = String

type Output = Int

data EnginePart = Digit Int | Symbol Char | Empty
  deriving (Show, Eq)

isSymbolPart :: EnginePart -> Bool
isSymbolPart (Symbol _) = True
isSymbolPart _ = False

samePart :: EnginePart -> EnginePart -> Bool
samePart (Digit _) (Digit _) = True
samePart (Symbol _) (Symbol _) = True
samePart Empty Empty = True
samePart _ _ = False

type Schematic = V.Vector (V.Vector EnginePart)

parseInput :: Input -> Schematic
parseInput = V.fromList . map parseLine . lines
  where
    parseLine :: String -> V.Vector EnginePart
    parseLine = V.fromList . map parseChar

    parseChar :: Char -> EnginePart
    parseChar c
      | isDigit c = Digit (digitToInt c)
      | c == '.' = Empty
      | otherwise = Symbol c

-- Returns a list of numbers that touch symbols
walkSchematic :: Schematic -> [Int]
walkSchematic schematic = concat $ V.imap walkLine schematic
  where
    walkLine :: Int -> V.Vector EnginePart -> [Int]
    walkLine row line = foldr go [] groupedWithIndices
      where
        groupedWithIndices :: [V.Vector (Int, EnginePart)]
        groupedWithIndices = V.groupBy (\a b -> samePart (snd a) (snd b)) $ V.indexed line

        go :: V.Vector (Int, EnginePart) -> [Int] -> [Int]
        go vec acc
          | V.null vec || (length digits /= V.length vec) = acc
          | otherwise =
            if any (\(col, _) -> touchesSymbol col row) vec
              then digitsToInt digits : acc
              else acc
          where
            digits :: [Int]
            digits =
              V.toList $
                V.mapMaybe
                  ( \case
                      (_, Digit d) -> Just d
                      _ -> Nothing
                  )
                  vec

    touchesSymbol :: Int -> Int -> Bool
    touchesSymbol col row =
      let coords =
            [ (col - 1, row - 1),
              (col, row -1),
              (col + 1, row -1),
              (col - 1, row),
              (col, row),
              (col + 1, row),
              (col - 1, row + 1),
              (col, row + 1),
              (col + 1, row + 1)
            ]
       in any isSymbolPart $ mapMaybe (schematic !?!?) coords

part1 :: Input -> Output
part1 = sum . walkSchematic . parseInput

part2 :: Input -> Output
part2 _ = 0
