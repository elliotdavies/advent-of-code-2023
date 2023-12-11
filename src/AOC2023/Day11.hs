module AOC2023.Day11
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Solution, findAllCoords, fromParser, manhattanDist)
import Data.List (sortOn)
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.Parsec (many, many1, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Object = Galaxy | Empty
  deriving (Eq)

type Grid = V.Vector (V.Vector Object)

object :: Parser Object
object = do
  c <- oneOf ".#"
  case c of
    '.' -> pure Empty
    '#' -> pure Galaxy
    _ -> error $ "Unexpected parse " ++ [c]

row :: Parser (V.Vector Object)
row = do
  os <- many1 object
  optional newline
  pure $ V.fromList os

grid :: Parser Grid
grid = V.fromList <$> many row

expand :: Int -> [Coords] -> [Coords]
expand multiplier cs = expandRows $ expandCols cs
  where
    expandCols = map (\(x, y) -> (x + (numEmptyBelow x * (multiplier - 1)), y)) . sortOn fst
      where
        numEmptyBelow :: Int -> Int
        numEmptyBelow c = length $ filter isEmpty [0 .. c - 1]

        isEmpty i = not $ S.member i filledCols

        filledCols = S.fromList $ map fst cs

    expandRows = map (\(x, y) -> (x, y + (numEmptyBelow y * (multiplier - 1)))) . sortOn snd
      where
        numEmptyBelow :: Int -> Int
        numEmptyBelow r = length $ filter isEmpty [0 .. r - 1]

        isEmpty i = not $ S.member i filledRows

        filledRows = S.fromList $ map snd cs

sumShortestPaths :: Int -> Grid -> Int
sumShortestPaths multiplier =
  sum . dists . expand multiplier . findAllCoords (== Galaxy)
  where
    dists :: [Coords] -> [Int]
    dists [] = []
    dists (cs : rest) = map (manhattanDist cs) rest ++ dists rest

part1 :: Solution
part1 = fromParser (sumShortestPaths 2) . parse grid ""

part2 :: Int -> Solution
part2 multiplier = fromParser (sumShortestPaths multiplier) . parse grid ""
