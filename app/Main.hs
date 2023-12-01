module Main where

import qualified AOC2023.Day01
import System.Environment

data Part
  = Part1
  | Part2
  deriving (Show, Read)

data Day
  = Day01
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs

  case args of
    (d : p : _) -> do
      let day :: Day = read d
      let part :: Part = read p

      case (day, part) of
        (Day01, Part1) -> print $ AOC2023.Day01.part1 AOC2023.Day01.input1
        (Day01, Part2) -> print $ AOC2023.Day01.part2 AOC2023.Day01.input2
    _ ->
      putStrLn "Missing arguments"
