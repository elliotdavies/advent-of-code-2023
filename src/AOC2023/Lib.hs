{- For Ormolu: -}
{-# LANGUAGE TypeApplications #-}

module AOC2023.Lib
  ( digitsToInt,
    (!?!?),
  )
where

import qualified Data.Vector as V

digitsToInt :: [Int] -> Int
digitsToInt = read @Int . concatMap show

(!?!?) :: V.Vector (V.Vector a) -> (Int, Int) -> Maybe a
(!?!?) vec (x, y) = vec V.!? y >>= \vec' -> vec' V.!? x
