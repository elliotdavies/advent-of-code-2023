module AOC2023.Day13
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, fromParser, transpose)
import qualified Data.Vector as V
import Text.Parsec (many1, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Ground = Ash | Rock
  deriving (Eq, Show)

type Grid = V.Vector (V.Vector Ground)

ground :: Parser Ground
ground = do
  c <- oneOf ".#"
  case c of
    '.' -> pure Ash
    '#' -> pure Rock
    _ -> error $ "Unexpected parse: " ++ [c]

grid :: Parser Grid
grid = do
  rows <- many1 $ do
    cols <- many1 ground
    optional newline
    pure $ V.fromList cols
  pure $ V.fromList rows

grids :: Parser [Grid]
grids = many1 $ do
  g <- grid
  optional newline
  pure g

findHorizontalReflection :: Grid -> Maybe Int -- num rows before reflection
findHorizontalReflection g = foldr go Nothing [1 .. V.length g - 1]
  where
    go _ (Just i) = Just i
    go i Nothing = if reflects i then Just i else Nothing

    reflects i =
      let (rs, rs') = V.splitAt i g
          (v1, v2) =
            if V.length rs <= V.length rs'
              then (V.reverse rs, rs')
              else (rs', V.reverse rs)
       in matches v1 v2

    matches v1 v2 = V.all id $ V.zipWith (==) v1 v2

findVerticalReflection :: Grid -> Maybe Int -- num cols before reflection
findVerticalReflection = findHorizontalReflection . transpose

part1 :: Solution
part1 = fromParser go . parse grids ""
  where
    go =
      sum
        . map
          ( \g -> case findHorizontalReflection g of
              Just i -> 100 * i
              Nothing -> case findVerticalReflection g of
                Just i -> i
                Nothing -> error $ "No reflection: " ++ show g
          )

part2 :: Solution
part2 = undefined
