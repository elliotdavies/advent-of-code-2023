module AOC2023.Day18
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Dir (..), Solution, digits, fromParser, polygonSize, travelN)
import Numeric (readHex)
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

followPlan :: [Instr] -> (Int, [Coords])
followPlan = foldr go (0, [(0, 0)]) . reverse
  where
    go :: Instr -> (Int, [Coords]) -> (Int, [Coords])
    go _ (n, []) = (n, [])
    go (dir, n, _) (numPoints, coords@(cs : _)) =
      let next = travelN n dir cs
       in (numPoints + n, next : coords)

part1 :: Solution
part1 = fromParser go . parse (many1 instr) ""
  where
    go instrs = let (n, cs) = followPlan instrs in polygonSize cs n

part2 :: Solution
part2 = fromParser go . parse (many1 instr) ""
  where
    go instrs =
      let (n, cs) = followPlan (map convertInstr instrs)
       in polygonSize cs n

    convertInstr :: Instr -> Instr
    convertInstr (_, _, hex) = (d, n, hex)
      where
        (d, n) = (toDir (last hex), fromHex (init hex))

        toDir '0' = R
        toDir '1' = D
        toDir '2' = L
        toDir '3' = U
        toDir _ = error "Invalid hex for dir"

        fromHex :: String -> Int
        fromHex = fst . head . readHex
