{-# LANGUAGE LambdaCase #-}

module AOC2023.Day03
  ( Input,
    Output,
    part1,
    part2,
  )
where

import AOC2023.Lib (Coords, digitsToInt, surroundingCoords, (!?!?))
import Data.Char (digitToInt, isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V

type Input = String

type Output = Int

data EnginePart = Digit Int | Symbol Char | Empty
  deriving (Show, Eq)

samePart :: EnginePart -> EnginePart -> Bool
samePart (Digit _) (Digit _) = True
samePart (Symbol _) (Symbol _) = True
samePart Empty Empty = True
samePart _ _ = False

data SymbolLoc = SymbolLoc Coords Char
  deriving (Show, Eq, Ord)

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

-- Returns a list of numbers that touch symbols, along with the symbol(s) they
-- touch
findPartNumbers :: Schematic -> [(Int, S.Set SymbolLoc)]
findPartNumbers schematic = concat $ V.imap walkLine schematic
  where
    walkLine :: Int -> V.Vector EnginePart -> [(Int, S.Set SymbolLoc)]
    walkLine row line = foldr go [] groupedWithIndices
      where
        groupedWithIndices :: [V.Vector (Int, EnginePart)]
        groupedWithIndices = V.groupBy (\a b -> samePart (snd a) (snd b)) $ V.indexed line

        go :: V.Vector (Int, EnginePart) -> [(Int, S.Set SymbolLoc)] -> [(Int, S.Set SymbolLoc)]
        go vec acc
          | (length digits /= V.length vec) || S.null uniqueTouchingSymbols = acc
          | otherwise = (digitsToInt digits, uniqueTouchingSymbols) : acc
          where
            uniqueTouchingSymbols :: S.Set SymbolLoc
            uniqueTouchingSymbols = mconcat $ V.toList $ V.map (\(col, _) -> getTouchingSymbols (col, row)) vec

            digits :: [Int]
            digits = V.toList $ V.mapMaybe (\case (_, Digit d) -> Just d; _ -> Nothing) vec

    getTouchingSymbols :: Coords -> S.Set SymbolLoc
    getTouchingSymbols (col, row) =
      foldr pickSymbols S.empty $ mapMaybe lookupWithCoords (surroundingCoords col row)
      where
        lookupWithCoords :: Coords -> Maybe (Coords, EnginePart)
        lookupWithCoords coords = (coords,) <$> (schematic !?!? coords)

        pickSymbols :: (Coords, EnginePart) -> S.Set SymbolLoc -> S.Set SymbolLoc
        pickSymbols (coords, Symbol c) acc = S.insert (SymbolLoc coords c) acc
        pickSymbols _ acc = acc

part1 :: Input -> Output
part1 = sum . map fst . findPartNumbers . parseInput

-- 'Invert' the list of mappings
mapSymbolLocsToPartNumbers :: [(Int, S.Set SymbolLoc)] -> M.Map SymbolLoc [Int]
mapSymbolLocsToPartNumbers = foldr go M.empty
  where
    go :: (Int, S.Set SymbolLoc) -> M.Map SymbolLoc [Int] -> M.Map SymbolLoc [Int]
    go (partNumber, symbols) acc =
      S.foldr (\sym acc' -> M.insertWith (++) sym [partNumber] acc') acc symbols

part2 :: Input -> Output
part2 input =
  let mapped = mapSymbolLocsToPartNumbers $ findPartNumbers $ parseInput input
      gears = M.filterWithKey (\(SymbolLoc _ c) vals -> c == '*' && length vals == 2) mapped
   in M.foldr ((+) . product) 0 gears
