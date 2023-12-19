module AOC2023.Day18
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Dir (..), Solution, digits, fromParser, polygonSize, travelN)
import Text.Parsec (char, many, many1, newline, noneOf, optional, parse, spaces)
import Text.Parsec.Char (oneOf)
import Text.Parsec.String (Parser)

type Instr = (Dir, Int, String)

instr :: Parser Instr
instr = do
  dir <- read . pure <$> oneOf "UDLR"
  spaces
  n <- digits
  spaces
  char '('
  char '#'
  hex <- many $ noneOf ")"
  char ')'
  optional newline
  pure (dir, n, hex)

followPlan :: [Instr] -> [Coords]
followPlan = init . foldr go [(0, 0)] . reverse
  where
    go :: Instr -> [Coords] -> [Coords]
    go _ [] = []
    go (dir, n, _) coords@(cs : _) =
      map (\i -> travelN i dir cs) (reverse [1 .. n]) ++ coords

part1 :: Solution
part1 = fromParser go . parse (many1 instr) ""
  where
    go = polygonSize . followPlan

part2 :: Solution
part2 = undefined
