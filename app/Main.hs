module Main where

import qualified AOC2023.Day01
import qualified AOC2023.Day02
import qualified AOC2023.Day03
import System.Environment

data Part
  = Part1
  | Part2
  deriving (Show, Read)

data Day
  = Day01
  | Day02
  | Day03
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs

  case args of
    (d : p : _) -> do
      let day :: Day = read d
      let part :: Part = read p

      case (day, part) of
        (Day01, Part1) -> do
          input <- readFile "inputs/day01/real.txt"
          print $ AOC2023.Day01.part1 input
        (Day01, Part2) -> do
          input <- readFile "inputs/day01/real.txt"
          print $ AOC2023.Day01.part2 input
        (Day02, Part1) -> do
          input <- readFile "inputs/day02/real.txt"
          print $ AOC2023.Day02.part1 input
        (Day02, Part2) -> do
          input <- readFile "inputs/day02/real.txt"
          print $ AOC2023.Day02.part2 input
        (Day03, Part1) -> do
          input <- readFile "inputs/day03/real.txt"
          print $ AOC2033.Day03.part1 input
        (Day03, Part2) -> do
          input <- readFile "inputs/day03/real.txt"
          print $ AOC2033.Day03.part2 input
    _ ->
      putStrLn "Missing arguments"
