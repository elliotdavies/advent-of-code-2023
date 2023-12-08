module AOC2023.Day08
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, fromParser, spaces)
import qualified Data.Map.Strict as M
import Text.Parsec (alphaNum, count, letter, many, newline, optional, parse)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

data Dir = L | R
  deriving (Eq, Read, Show)

newtype Node = Node String
  deriving (Eq, Ord, Show)

type Network = M.Map Node (Node, Node)

data Input = Input [Dir] Network

parseDirs :: Parser [Dir]
parseDirs = do
  ls <- many letter
  pure $ read . pure <$> ls

network :: Parser Network
network = do
  key <- count 3 alphaNum
  spaces
  char '='
  spaces
  char '('
  left <- count 3 alphaNum
  char ','
  spaces
  right <- count 3 alphaNum
  char ')'
  pure $ M.fromList [(Node key, (Node left, Node right))]

input :: Parser Input
input = do
  ds <- parseDirs
  newline
  newline
  ns <- many $ do
    n <- network
    optional newline
    pure n
  pure $ Input ds $ foldr M.union M.empty ns

followMap :: Node -> Input -> Int
followMap start (Input dirs nw) = snd $ go (cycle dirs) (start, 0)
  where
    go :: [Dir] -> (Node, Int) -> (Node, Int)
    go (d : ds) (node, n)
      | isEnd node = (node, n)
      | otherwise =
        let (l, r) = nw M.! node
         in go ds (if d == L then l else r, n + 1)
    go [] acc = acc

    isEnd (Node xs) = last xs == 'Z'

part1 :: Solution
part1 = fromParser (followMap (Node "AAA")) . parse input ""

part2 :: Solution
part2 = fromParser followMaps . parse input ""
  where
    followMaps :: Input -> Int
    followMaps inp@(Input _ nw) = foldr1 lcm $ map (`followMap` inp) starts
      where
        starts :: [Node] = filter (\(Node xs) -> last xs == 'A') $ M.keys nw
