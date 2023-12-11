module AOC2023.Day10
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Dir (..), Solution, coordsOf, fromParser, opposite, safeHead, travel, (!?!?))
import Control.Monad (join)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.Parsec (anyChar, many, manyTill, newline, optional, parse)
import Text.Parsec.String (Parser)

data Pipe
  = P [Dir] -- Directions in which the pipe can connect
  | S -- Start
  deriving (Eq)

type Grid = V.Vector (V.Vector (Maybe Pipe))

pipe :: Parser (Maybe Pipe)
pipe = do
  c <- anyChar
  pure $ case c of
    '|' -> Just $ P [U, D]
    '-' -> Just $ P [L, R]
    'L' -> Just $ P [U, R]
    'J' -> Just $ P [U, L]
    'F' -> Just $ P [D, R]
    '7' -> Just $ P [D, L]
    'S' -> Just S
    _ -> Nothing

row :: Parser (V.Vector (Maybe Pipe))
row = V.fromList <$> pipe `manyTill` newline

grid :: Parser Grid
grid = do
  rows <- many $ do
    r <- row
    optional newline
    pure r
  pure $ V.fromList rows

followPipe :: Grid -> (S.Set Coords, Coords) -> (S.Set Coords, Coords)
followPipe g (seen, cs) =
  case nextCoords of
    Just next -> followPipe g (seen', next)
    Nothing -> (seen', cs)
  where
    nextCoords :: Maybe Coords
    nextCoords =
      safeHead $
        filter (\cs' -> not $ S.member cs' seen) $
          map (`travel` cs) $
            filter canConnect [U, D, L, R]

    canConnect :: Dir -> Bool
    canConnect d =
      case (currentPipe, targetPipe) of
        (Just (P cxns), Just (P cxns')) -> d `elem` cxns && opposite d `elem` cxns'
        -- The start pipe can connect to any valid pipe
        (Just S, Just (P cxns')) -> opposite d `elem` cxns'
        -- Any pipe can connect to the start pipe
        (_, Just S) -> True
        (Nothing, _) -> False
        (_, Nothing) -> False
      where
        currentPipe = join $ g !?!? cs
        targetPipe = join $ g !?!? travel d cs

    seen' :: S.Set Coords
    seen' = S.insert cs seen

part1 :: Solution
part1 = fromParser go . parse grid ""
  where
    go :: Grid -> Int
    go g
      | isJust start =
        let (seen, _) = followPipe g (S.empty, fromJust start)
         in S.size seen `div` 2
      | otherwise = error "Couldn't find start"
      where
        start = coordsOf (Just S) g

part2 :: Solution
part2 = undefined
