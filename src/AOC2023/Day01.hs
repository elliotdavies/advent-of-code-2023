{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Day01
  ( Input,
    Output,
    part1,
    part2,
  )
where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Text.Read (readMaybe)

findNums :: String -> [Int]
findNums = catMaybes . parse []
  where
    parse :: [Maybe Int] -> String -> [Maybe Int]
    parse acc (x : xs) =
      let res = parseStr (x : xs) <|> parseDigit x
       in parse (acc ++ [res]) xs
    parse acc [] = acc

parseDigit :: Char -> Maybe Int
parseDigit = readMaybe . pure

parseStr :: String -> Maybe Int
parseStr s = foldr go Nothing pairs
  where
    go :: (String, Int) -> Maybe Int -> Maybe Int
    go pair acc
      | isJust acc = acc
      | fst pair `isPrefixOf` s = Just $ snd pair
      | otherwise = Nothing

    pairs :: [(String, Int)]
    pairs =
      [ ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9)
      ]

calibrationValue :: [Int] -> Int
calibrationValue nums = read @Int $ show (head nums) ++ show (last nums)

type Input = String

type Output = Int

part1 :: Input -> Output
part1 = sum . map (calibrationValue . mapMaybe parseDigit) . lines

part2 :: Input -> Output
part2 = sum . map (calibrationValue . findNums) . lines
