module AOC2023.Day09
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, fromParser, spaceSeparatedDigits1)
import Text.Parsec (many, newline, optional, parse)
import Text.Parsec.String (Parser)

diffNums :: [Int] -> [Int]
diffNums = tail . snd . foldl (\(prev, acc) x -> (x, acc ++ [x - prev])) (0, [])

-- Forwards
project :: [Int] -> Int
project xs
  | all (== 0) diffs = last xs
  | otherwise = last xs + project diffs
  where
    diffs = diffNums xs

-- Backwards
project' :: [Int] -> Int
project' xs
  | all (== 0) diffs = head xs
  | otherwise = head xs - project' diffs
  where
    diffs = diffNums xs

input :: Parser [[Int]]
input = many $ do
  ds <- spaceSeparatedDigits1
  optional newline
  pure ds

part1 :: Solution
part1 = fromParser (sum . map project) . parse input ""

part2 :: Solution
part2 = fromParser (sum . map project') . parse input ""
