{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Lib where

import qualified Data.Vector as V

digitsToInt :: [Int] -> Int
digitsToInt = read @Int . concatMap show

type Coords = (Int, Int)

(!?!?) :: V.Vector (V.Vector a) -> Coords -> Maybe a
(!?!?) vec (x, y) = vec V.!? y >>= \vec' -> vec' V.!? x

surroundingCoords :: Int -> Int -> [Coords]
surroundingCoords col row =
  [ (col - 1, row - 1),
    (col, row -1),
    (col + 1, row -1),
    (col - 1, row),
    -- (col, row),
    (col + 1, row),
    (col - 1, row + 1),
    (col, row + 1),
    (col + 1, row + 1)
  ]
