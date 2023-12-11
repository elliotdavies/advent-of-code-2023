{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Lib where

import Data.Bifunctor (Bifunctor (bimap))
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

{- Coords and vectors -}

type Coords = (Int, Int)

-- Index safely into a 2D vector
(!?!?) :: V.Vector (V.Vector a) -> Coords -> Maybe a
(!?!?) vec (x, y) = vec V.!? y >>= \vec' -> vec' V.!? x

-- Find the coordinates of an item in a 2D vector
coordsOf :: Eq a => a -> V.Vector (V.Vector a) -> Maybe Coords
coordsOf x = V.ifoldr findInRow Nothing
  where
    findInRow rowIdx row Nothing = (,rowIdx) <$> V.findIndex (x ==) row
    findInRow _ _ acc = acc

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

{- Directions -}

data Dir = U | D | L | R
  deriving (Eq, Show)

opposite :: Dir -> Dir
opposite U = D
opposite D = U
opposite L = R
opposite R = L

travel :: Dir -> Coords -> Coords
travel U (x, y) = (x, y -1)
travel D (x, y) = (x, y + 1)
travel L (x, y) = (x - 1, y)
travel R (x, y) = (x + 1, y)
