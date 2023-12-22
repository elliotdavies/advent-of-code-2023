module AOC2023.Day21
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Dir (..), Solution, findCoords, fromParser, travel, (!?!?))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Text.Parsec (many1, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Tile = Start | Plot | Rock
  deriving (Eq)

type Grid = V.Vector (V.Vector Tile)

tile :: Parser Tile
tile =
  oneOf "S.#" <&> \case
    'S' -> Start
    '.' -> Plot
    '#' -> Rock
    _ -> error "Bad tile"

input :: Parser Grid
input = do
  rows <- many1 $ do
    tiles <- many1 tile
    optional newline
    pure $ V.fromList tiles
  pure $ V.fromList rows

part1 :: Int -> Solution
part1 steps = fromParser go . parse input ""
  where
    go grid = M.size $ M.filter even $ walk (M.empty, [(start, 0)])
      where
        -- breadth-first search
        walk (seen, []) = seen
        walk (seen, (nextCs, n) : toSee)
          | nextCs `M.member` seen = walk (seen, toSee)
          | n > steps = walk (seen, toSee)
          | otherwise =
            let seen' = M.insert nextCs n seen
                toSee' = map (,n + 1) $ nextSteps nextCs
             in walk (seen', toSee ++ toSee')

        nextSteps cs =
          filter
            ( \cs' -> case grid !?!? cs' of
                Just Start -> True
                Just Plot -> True
                _ -> False
            )
            (map (`travel` cs) [U .. R])

        start = fromJust $ findCoords (== Start) grid

part2 :: Solution
part2 = undefined
