module AOC2023.Day13
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, allCoords, fromParser, transpose, updateAt)
import Control.Applicative ((<|>))
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector as V
import Text.Parsec (many1, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Ground = Ash | Rock
  deriving (Eq, Show)

type Grid = V.Vector (V.Vector Ground)

data ReflectionDirection = Horizontal | Vertical
  deriving (Eq, Show)

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

findHorizontalReflection :: Maybe Int -> Grid -> Maybe Int -- num rows before reflection
findHorizontalReflection excl g = foldr go Nothing [1 .. V.length g - 1]
  where
    go _ (Just i) = Just i
    go i Nothing = if reflects i && i /= fromMaybe 0 excl then Just i else Nothing

    reflects i =
      let (rs, rs') = V.splitAt i g
          (v1, v2) =
            if V.length rs <= V.length rs'
              then (V.reverse rs, rs')
              else (rs', V.reverse rs)
       in matches v1 v2

    matches v1 v2 = V.all id $ V.zipWith (==) v1 v2

findVerticalReflection :: Maybe Int -> Grid -> Maybe Int -- num cols before reflection
findVerticalReflection excl = findHorizontalReflection excl . transpose

findReflection :: Maybe (ReflectionDirection, Int) -> Grid -> Maybe (ReflectionDirection, Int)
findReflection excl g =
  ((Horizontal,) <$> findHorizontalReflection exclH g)
    <|> ((Vertical,) <$> findVerticalReflection exclV g)
  where
    exclH = excl >>= \(dir, i) -> if dir == Horizontal then Just i else Nothing
    exclV = excl >>= \(dir, i) -> if dir == Vertical then Just i else Nothing

part1 :: Solution
part1 = fromParser go . parse grids ""
  where
    go =
      sum
        . map
          ( \g -> case findReflection Nothing g of
              Just (Horizontal, i) -> i * 100
              Just (Vertical, i) -> i
              Nothing -> error $ "No reflection: " ++ show g
          )

part2 :: Solution
part2 = fromParser go . parse grids ""
  where
    go =
      sum
        . map
          ( \g -> case findNewReflection g of
              Just (Horizontal, i) -> i * 100
              Just (Vertical, i) -> i
              Nothing -> error $ "No new reflection: " ++ show g
          )

    findNewReflection :: Grid -> Maybe (ReflectionDirection, Int)
    findNewReflection g =
      case nub $ mapMaybe (findReflection originalReflection) $ variants g of
        [] -> Nothing
        [r] -> Just r
        rs -> error $ "Multiple reflections: " ++ show rs
      where
        originalReflection = findReflection Nothing g

    variants :: Grid -> [Grid]
    variants g = map flipGround $ allCoords g
      where
        flipGround = updateAt (\item -> if item == Ash then Rock else Ash) g
