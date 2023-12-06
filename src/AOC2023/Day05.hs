{-# LANGUAGE RecordWildCards #-}

module AOC2023.Day05
  ( part1,
    part2,
    seedsParser,
    rangeParser,
    mapParser,
    Range (..),
    Map (..),
  )
where

import AOC2023.Lib (Solution, fromParser, spaceSeparatedDigits, spaces)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.Parsec (char, many, newline, noneOf, optional, parse, string)
import Text.Parsec.String (Parser)

type Seed = Int

type Location = Int

data Range = Range
  { destRangeStart :: Int,
    srcRangeStart :: Int,
    rangeLength :: Int
  }
  deriving (Eq, Show)

newtype Map = Map [Range]
  deriving (Eq, Show)

data Input = Input {seeds :: [Seed], maps :: [Map]}
  deriving (Show)

seedsParser :: Parser [Seed]
seedsParser = do
  string "seeds:"
  spaces
  spaceSeparatedDigits

skipToNextMap :: Parser ()
skipToNextMap = do
  many $ noneOf ":"
  char ':'
  newline
  pure ()

rangeParser :: Parser Range
rangeParser = do
  (destRangeStart : srcRangeStart : rangeLength : _) <- spaceSeparatedDigits
  pure $ Range {..}

mapParser :: Parser Map
mapParser = do
  ranges <- many $ do
    range <- rangeParser
    optional newline
    return range
  pure $ Map ranges

inputParser :: Parser Input
inputParser = do
  seeds <- seedsParser
  maps <- many $ do
    skipToNextMap
    mapParser
  pure $ Input {..}

data Direction = Forwards | Backwards
  deriving (Eq)

runMap :: Direction -> Map -> Int -> Int
runMap dir (Map ranges) n = fromMaybe n $ foldr go Nothing ranges
  where
    go :: Range -> Maybe Int -> Maybe Int
    go _ (Just b) = Just b
    go (Range {..}) _
      | dir == Forwards && n >= srcRangeStart && n < srcRangeStart + rangeLength =
        Just $ n + destRangeStart - srcRangeStart
      | dir == Backwards && n >= destRangeStart && n < destRangeStart + rangeLength =
        Just $ n + srcRangeStart - destRangeStart
      | otherwise = Nothing

part1 :: Solution
part1 = fromParser findNearestLoc . parse inputParser ""
  where
    findNearestLoc :: Input -> Int
    findNearestLoc (Input {..}) =
      minimum $ map (\seed -> foldr (runMap Forwards) seed $ reverse maps) seeds

part2 :: Solution
part2 = fromParser findFirstLoc . parse inputParser ""
  where
    findFirstLoc :: Input -> Int
    findFirstLoc (Input {..}) = maybe (-1) fst firstValidSeed
      where
        firstValidSeed :: Maybe (Location, Seed)
        firstValidSeed =
          find isValidSeed $
            map
              (\loc -> (loc, foldr (runMap Backwards) loc maps))
              [0 .. 1_000_000_000]

        isValidSeed :: (Location, Seed) -> Bool
        isValidSeed (_, seed) =
          any
            (\(start, len) -> seed >= start && seed < start + len)
            (seedRanges seeds)

        seedRanges :: [Seed] -> [(Int, Int)]
        seedRanges (start : len : rest) = (start, len) : seedRanges rest
        seedRanges _ = []
