{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Lib where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.List as L
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Text.Parsec (ParseError, digit, many, many1, optionMaybe)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

{- Puzzle setup -}

type Result = Either String Int

type Solution = String -> Result

{- General helpers -}

-- Convert e.g. [1,2,3] to 123
digitsToInt :: [Int] -> Int
digitsToInt = read @Int . concatMap show

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead _ = Nothing

{- Parsers -}

digits :: Parser Int
digits = do
  c <- optionMaybe $ char '-'
  ds <- many1 digit
  pure $ read $ if isJust c then '-' : ds else ds

spaces :: Parser String
spaces = many $ char ' '

spaceSeparatedDigits :: Parser [Int]
spaceSeparatedDigits = many $ do
  ds <- digits
  spaces
  pure ds

spaceSeparatedDigits1 :: Parser [Int]
spaceSeparatedDigits1 = many1 $ do
  ds <- digits
  spaces
  pure ds

fromParser :: forall a. (a -> Int) -> Either ParseError a -> Result
fromParser = bimap show

{- Coords -}

type Coords = (Int, Int)

manhattanDist :: Coords -> Coords -> Int
manhattanDist (x, y) (x', y') = abs (x - x') + abs (y - y')

-- Generate coordinates for the cells surrounding the current one (but not
-- including the current one)
surroundingCoords :: Coords -> [Coords]
surroundingCoords (x, y) =
  [ (x - 1, y - 1),
    (x, y -1),
    (x + 1, y -1),
    (x - 1, y),
    -- (x, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

-- Shoelace formula for area of polygon
shoelace :: [Coords] -> Int
shoelace vertices =
  let xsByYs = zipWith (*) xs (tail ys ++ [head ys])
      ysByXs = zipWith (*) ys (tail xs ++ [head xs])
   in abs (sum ysByXs - sum xsByYs) `div` 2
  where
    xs = map fst vertices
    ys = map snd vertices

-- Pick's algorithm
-- TODO Rewrite Day10 to use this
polygonSize :: [Coords] -> Int -> Int
polygonSize vertices boundaryLength = interiorSize + boundaryLength
  where
    interiorSize = area - (boundaryLength `div` 2) + 1
    area = shoelace vertices

{- Vectors -}

-- Index safely into a 2D vector
(!?!?) :: V.Vector (V.Vector a) -> Coords -> Maybe a
(!?!?) vec (x, y) = vec V.!? y >>= \vec' -> vec' V.!? x

updateAt :: (a -> a) -> V.Vector (V.Vector a) -> Coords -> V.Vector (V.Vector a)
updateAt f g (x, y) =
  let row = g V.! y
      item = row V.! x
      row' = row V.// [(x, f item)]
   in g V.// [(y, row')]

dims :: V.Vector (V.Vector a) -> (Int, Int)
dims g = (V.length (g V.! 0), V.length g)

allCoords :: V.Vector (V.Vector a) -> [Coords]
allCoords g = [(x, y) | x <- [0 .. maxX - 1], y <- [0 .. maxY - 1]]
  where
    (maxX, maxY) = dims g

transpose :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transpose = V.fromList . map V.fromList . L.transpose . V.toList . V.map V.toList

rotateClockwise :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
rotateClockwise = transpose . V.map V.reverse

-- Find the coordinates of the first matching item in a 2D vector
findCoords :: Eq a => (a -> Bool) -> V.Vector (V.Vector a) -> Maybe Coords
findCoords f = V.ifoldr findInRow Nothing
  where
    findInRow rowIdx row Nothing = (,rowIdx) <$> V.findIndex f row
    findInRow _ _ acc = acc

-- Find all coordinates of matching items in a 2D vector
findAllCoords :: Eq a => (a -> Bool) -> V.Vector (V.Vector a) -> [Coords]
findAllCoords f = V.ifoldr findInRow []
  where
    findInRow rowIdx row acc = acc ++ ((,rowIdx) <$> V.toList (V.findIndices f row))

{- Directions -}

data Dir = U | D | L | R
  deriving (Eq, Show, Read, Ord, Enum)

opposite :: Dir -> Dir
opposite U = D
opposite D = U
opposite L = R
opposite R = L

travelN :: Int -> Dir -> Coords -> Coords
travelN n U (x, y) = (x, y - n)
travelN n D (x, y) = (x, y + n)
travelN n L (x, y) = (x - n, y)
travelN n R (x, y) = (x + n, y)

travel :: Dir -> Coords -> Coords
travel = travelN 1
