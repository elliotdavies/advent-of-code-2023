{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day05
  ( part1,
    part2,
    seedsParser,
    rangeParser,
    mapParser,
    Seed (..),
    Range (..),
    Map (..),
  )
where

import AOC2023.Lib (Solution, digits, fromParser)
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.Parsec (char, many, newline, noneOf, optional, parse, string)
import Text.Parsec.String (Parser)

class Category c where
  wrap :: Int -> c
  unwrap :: c -> Int

newtype Seed = Seed Int
  deriving (Eq, Show)

instance Category Seed where
  wrap = coerce
  unwrap = coerce

newtype Soil = Soil Int

instance Category Soil where
  wrap = coerce
  unwrap = coerce

newtype Fertilizer = Fertilizer Int

instance Category Fertilizer where
  wrap = coerce
  unwrap = coerce

newtype Water = Water Int

instance Category Water where
  wrap = coerce
  unwrap = coerce

newtype Light = Light Int

instance Category Light where
  wrap = coerce
  unwrap = coerce

newtype Temperature = Temperature Int

instance Category Temperature where
  wrap = coerce
  unwrap = coerce

newtype Humidity = Humidity Int

instance Category Humidity where
  wrap = coerce
  unwrap = coerce

newtype Location = Location Int

instance Category Location where
  wrap = coerce
  unwrap = coerce

data Range = Range
  { destRangeStart :: Int,
    srcRangeStart :: Int,
    rangeLength :: Int
  }
  deriving (Eq, Show)

newtype Map a b = Map [Range]
  deriving (Eq, Show)

data Input = Input
  { seeds :: [Seed],
    seedSoilMap :: Map Seed Soil,
    soilFertilizerMap :: Map Soil Fertilizer,
    fertilizerWaterMap :: Map Fertilizer Water,
    waterLightMap :: Map Water Light,
    lightTemperatureMap :: Map Light Temperature,
    temperatureHumidityMap :: Map Temperature Humidity,
    humidityLocationMap :: Map Humidity Location
  }
  deriving (Show)

spaceSeparatedDigits :: Parser [Int]
spaceSeparatedDigits = many $ do
  ds <- digits
  many $ char ' '
  pure ds

seedsParser :: Parser [Seed]
seedsParser = do
  string "seeds:"
  many $ char ' '
  ds <- spaceSeparatedDigits
  pure $ Seed <$> ds

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

mapParser :: forall a b. Parser (Map a b)
mapParser = do
  ranges <- many $ do
    range <- rangeParser
    optional newline
    return range

  pure $ Map ranges

inputParser :: Parser Input
inputParser = do
  seeds <- seedsParser
  skipToNextMap
  seedSoilMap <- mapParser
  skipToNextMap
  soilFertilizerMap <- mapParser
  skipToNextMap
  fertilizerWaterMap <- mapParser
  skipToNextMap
  waterLightMap <- mapParser
  skipToNextMap
  lightTemperatureMap <- mapParser
  skipToNextMap
  temperatureHumidityMap <- mapParser
  skipToNextMap
  humidityLocationMap <- mapParser
  pure $ Input {..}

part1 :: Solution
part1 = fromParser findNearestLoc . parse inputParser ""
  where
    findNearestLoc :: Input -> Int
    findNearestLoc inp = minimum $ map (unwrap . chainConvert inp) (seeds inp)

    chainConvert :: Input -> Seed -> Location
    chainConvert (Input {..}) =
      convert humidityLocationMap
        . convert temperatureHumidityMap
        . convert lightTemperatureMap
        . convert waterLightMap
        . convert fertilizerWaterMap
        . convert soilFertilizerMap
        . convert seedSoilMap

    convert :: (Category a, Category b) => Map a b -> a -> b
    convert (Map ranges) cat = wrap $ fromMaybe n $ foldr go Nothing ranges
      where
        n :: Int
        n = unwrap cat

        go :: Range -> Maybe Int -> Maybe Int
        go _ (Just b) = Just b
        go (Range {..}) _
          | n >= srcRangeStart && n < srcRangeStart + rangeLength = Just $ n + offset
          | otherwise = Nothing
          where
            offset = destRangeStart - srcRangeStart

part2 :: Solution
part2 = fromParser findFirstSeed . parse inputParser ""
  where
    findFirstSeed :: Input -> Int
    findFirstSeed inp = maybe (-1) (unwrap . fst) firstValidSeed
      where
        firstValidSeed = find validSeed $ map (\loc -> (loc, chainConvert' inp loc)) (Location <$> [0 .. 1_000_000_000])

        validSeed :: (Location, Seed) -> Bool
        validSeed (_, Seed i) = any (\(start, len) -> i >= start && i < start + len) ranges

        ranges = seedRanges $ seeds inp

        seedRanges :: [Seed] -> [(Int, Int)]
        seedRanges (Seed start : Seed len : rest) = (start, len) : seedRanges rest
        seedRanges _ = []

    chainConvert' :: Input -> Location -> Seed
    chainConvert' (Input {..}) =
      convert' seedSoilMap
        . convert' soilFertilizerMap
        . convert' fertilizerWaterMap
        . convert' waterLightMap
        . convert' lightTemperatureMap
        . convert' temperatureHumidityMap
        . convert' humidityLocationMap

    convert' :: (Category a, Category b) => Map a b -> b -> a
    convert' (Map ranges) cat = wrap $ fromMaybe n $ foldr go Nothing ranges
      where
        n :: Int
        n = unwrap cat

        go :: Range -> Maybe Int -> Maybe Int
        go _ (Just b) = Just b
        go (Range {..}) _
          | destRangeStart <= n && n < destRangeStart + rangeLength = Just $ n + offset
          | otherwise = Nothing
          where
            offset = srcRangeStart - destRangeStart
