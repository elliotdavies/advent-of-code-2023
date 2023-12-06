module AOC2023.Day06
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digitsToInt, fromParser, spaceSeparatedDigits, spaces)
import Text.Parsec (newline, parse)
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser)

type Race = (Int, Int)

races :: Parser [Race]
races = do
  string "Time:"
  spaces
  times <- spaceSeparatedDigits
  newline
  string "Distance:"
  spaces
  zip times <$> spaceSeparatedDigits

oneBigRace :: Parser Race
oneBigRace = do
  string "Time:"
  spaces
  time <- digitsToInt <$> spaceSeparatedDigits
  newline
  string "Distance:"
  spaces
  dist <- digitsToInt <$> spaceSeparatedDigits
  pure (time, dist)

countWinOptions :: [Race] -> Int
countWinOptions = product . map (length . winOptions)
  where
    winOptions :: Race -> [Int]
    winOptions (raceTime, distToBeat) =
      filter (> distToBeat) $ map distTravelled [1 .. raceTime -1]
      where
        distTravelled timeHeld = (raceTime - timeHeld) * timeHeld

part1 :: Solution
part1 = fromParser countWinOptions . parse races ""

part2 :: Solution
part2 = fromParser (countWinOptions . pure) . parse oneBigRace ""
