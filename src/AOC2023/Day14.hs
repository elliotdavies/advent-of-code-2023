module AOC2023.Day14 where

-- ( part1,
--   part2,
-- )

import AOC2023.Lib (Solution, fromParser, rotateClockwise, transpose)
import qualified Data.Vector as V
import Text.Parsec (many1, newline, octDigit, optional, parse)
import Text.Parsec.Char (oneOf)
import Text.Parsec.String (Parser)

data Rock = Round | Cube | Empty
  deriving (Eq, Show)

type Platform = V.Vector (V.Vector Rock)

rock :: Parser Rock
rock = do
  c <- oneOf "O#."
  pure $ case c of
    'O' -> Round
    '#' -> Cube
    '.' -> Empty
    _ -> error $ "Unexpected parse: " ++ [c]

platform :: Parser Platform
platform = do
  rows <- many1 $ do
    rocks <- many1 rock
    optional newline
    pure $ V.fromList rocks
  pure $ transpose $ V.fromList rows

chunk :: [V.Vector Rock] -> V.Vector Rock -> [V.Vector Rock]
chunk acc v
  | V.null v = acc
  | otherwise =
    let (notCubes, rest) = V.span (/= Cube) v
        (cubes, rest') = V.span (== Cube) rest
     in chunk (acc ++ [notCubes, cubes]) rest'

tiltChunk :: V.Vector Rock -> V.Vector Rock
tiltChunk v
  | V.null v = v
  | v V.! 0 == Cube = v
  | otherwise =
    let numRound = V.length $ V.filter (== Round) v
        numEmpty = V.length v - numRound
     in V.replicate numRound Round V.++ V.replicate numEmpty Empty

calcLoad :: Platform -> Int
calcLoad p =
  V.sum $
    V.imap
      (\i row -> V.length (V.filter (== Round) row) * (V.length p - i))
      $ transpose p

part1 :: Solution
part1 = fromParser (calcLoad . V.map go) . parse platform ""
  where
    go = V.concat . map tiltChunk . chunk []

-- 1_000_000_000
part2 :: Solution
part2 = fromParser go . parse platform ""
  where
    go original = go' original 0
      where
        go' p i =
          let new = cyclePlatform p
           in if new == original
                then i + 1
                else go' new (i + 1)

        tilt = V.map (V.concat . map tiltChunk . chunk [])

        cyclePlatform =
          rotateClockwise
            . tilt
            . rotateClockwise
            . tilt
            . rotateClockwise
            . tilt
            . rotateClockwise
            . tilt
