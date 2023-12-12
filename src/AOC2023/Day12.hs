module AOC2023.Day12
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digits, fromParser)
import Data.List (group)
import Text.Parsec (char, many, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Condition
  = Operational
  | Damaged
  | Unknown
  deriving (Eq, Show)

data Row = Row [Condition] [Int]
  deriving (Show)

condition :: Parser Condition
condition = do
  c <- oneOf "?#."
  case c of
    '?' -> pure Unknown
    '#' -> pure Damaged
    '.' -> pure Operational
    _ -> error $ "Unexpected parse: " ++ [c]

sizes :: Parser [Int]
sizes = many $ do
  ds <- digits
  optional $ char ','
  pure ds

row :: Parser Row
row = do
  cs <- many condition
  char ' '
  ss <- sizes
  optional newline
  pure $ Row cs ss

rowValid :: Row -> Bool
rowValid (Row cs ss) =
  length chunks == length ss
    && all (\(chunk, size) -> length chunk == size) (zip chunks ss)
  where
    chunks = filter damaged $ group cs

    damaged (Damaged : _) = True
    damaged _ = False

arrangements :: Row -> [[Condition]]
arrangements (Row cs' ss) =
  filter (\cs -> rowValid (Row cs ss)) $ map replaceAll options
  where
    replaceAll :: [Condition] -> [Condition]
    replaceAll = go cs'
      where
        go :: [Condition] -> [Condition] -> [Condition]
        go [] _ = []
        go cs [] = cs
        go (Unknown : cs) (opt : opts) = opt : go cs opts
        go (c : cs) opts = c : go cs opts

    options :: [[Condition]]
    options = mapM (const [Operational, Damaged]) unknownIdxs

    unknownIdxs = map snd $ filter (\(c, _) -> c == Unknown) $ zip cs' [0 :: Int ..]

part1 :: Solution
part1 = fromParser go . parse (many row) ""
  where
    go :: [Row] -> Int
    go = sum . map (length . arrangements)

part2 :: Solution
part2 = undefined
