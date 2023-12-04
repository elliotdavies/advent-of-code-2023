module Main where

import qualified AOC2023.Day01
import qualified AOC2023.Day02
import qualified AOC2023.Day03
import qualified AOC2023.Day04
import AOC2023.Lib (Solution)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import System.Environment

data Part
  = Part1
  | Part2
  deriving (Show, Read, Ord, Eq)

data Day
  = Day01
  | Day02
  | Day03
  | Day04
  deriving (Show, Read, Ord, Eq)

dayToFolder :: Day -> String
dayToFolder day = let s = show day in toLower (head s) : tail s

mapping :: M.Map (Day, Part) Solution
mapping =
  M.fromList
    [ ((Day01, Part1), AOC2023.Day01.part1),
      ((Day01, Part2), AOC2023.Day01.part2),
      ((Day02, Part1), AOC2023.Day02.part1),
      ((Day02, Part2), AOC2023.Day02.part2),
      ((Day03, Part1), AOC2023.Day03.part1),
      ((Day03, Part2), AOC2023.Day03.part2),
      ((Day04, Part1), AOC2023.Day04.part1),
      ((Day04, Part2), AOC2023.Day04.part2)
    ]

main :: IO ()
main = do
  args <- getArgs

  case args of
    (d : p : _) -> do
      let day :: Day = read d
      let part :: Part = read p

      input <- readFile $ "inputs/" ++ dayToFolder day ++ "/real.txt"

      case M.lookup (day, part) mapping of
        Just f -> print $ f input
        Nothing -> print "Invalid day or part"
    _ ->
      print "Missing arguments"
