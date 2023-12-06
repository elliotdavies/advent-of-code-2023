{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Lib where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Vector as V
import Text.Parsec (ParseError, digit, many, many1)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

{- Puzzle setup -}

type Result = Either String Int

type Solution = String -> Result

{- General helpers -}

-- Convert e.g. [1,2,3] to 123
digitsToInt :: [Int] -> Int
digitsToInt = read @Int . concatMap show

{- Parsers -}

digits :: Parser Int
digits = read <$> many1 digit

spaces :: Parser String
spaces = many $ char ' '

spaceSeparatedDigits :: Parser [Int]
spaceSeparatedDigits = many $ do
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
