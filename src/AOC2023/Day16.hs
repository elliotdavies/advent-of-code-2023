module AOC2023.Day16
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Dir (..), Solution, dims, fromParser, travel, (!?!?))
import Data.Functor ((<&>))
import qualified Data.Set as S
import qualified Data.Vector as V
import Text.Parsec (many1, newline, oneOf, optional, parse)
import Text.Parsec.String (Parser)

data Object
  = Empty
  | MirrorF -- /
  | MirrorB -- \
  | SplitterV -- pipe (|)
  | SplitterH -- bar (-)

type Grid = V.Vector (V.Vector Object)

object :: Parser Object
object =
  oneOf "/\\|-." <&> \case
    '/' -> MirrorF
    '\\' -> MirrorB
    '|' -> SplitterV
    '-' -> SplitterH
    '.' -> Empty
    c -> error $ "Unexpected parse: " ++ [c]

row :: Parser (V.Vector Object)
row = do
  objs <- many1 object
  optional newline
  pure $ V.fromList objs

input :: Parser Grid
input = V.fromList <$> many1 row

type Heading = (Coords, Dir)

followBeams :: Grid -> Coords -> Dir -> S.Set Heading
followBeams g initCoords initDir = go S.empty [(initCoords, initDir)]
  where
    go :: S.Set Heading -> [Heading] -> S.Set Heading
    go seen [] = seen
    go seen ((cs, d) : splits) =
      let (seen', splits') = followBeam cs d (seen, [])
       in go seen' (splits ++ splits')

    followBeam :: Coords -> Dir -> (S.Set Heading, [Heading]) -> (S.Set Heading, [Heading])
    followBeam cs d acc@(seen, splits)
      | S.member (cs, d) seen = acc
      | otherwise =
        case g !?!? cs of
          Just Empty -> continue d
          Just MirrorF ->
            continue $ case d of
              U -> R
              R -> U
              D -> L
              L -> D
          Just MirrorB ->
            continue $ case d of
              U -> L
              L -> U
              D -> R
              R -> D
          Just SplitterV ->
            if d == L || d == R
              then (seen', splits ++ [(travel U cs, U), (travel D cs, D)])
              else continue d
          Just SplitterH ->
            if d == U || d == D
              then (seen', splits ++ [(travel L cs, L), (travel R cs, R)])
              else continue d
          Nothing -> acc
      where
        continue d' = followBeam (travel d' cs) d' (seen', splits)

        seen' = S.insert (cs, d) seen

countEnergised :: Grid -> Heading -> Int
countEnergised g (cs, d) = S.size $ S.map fst $ followBeams g cs d

part1 :: Solution
part1 = fromParser (`countEnergised` ((0, 0), R)) . parse input ""

part2 :: Solution
part2 = fromParser go . parse input ""
  where
    go g = maximum $ map (countEnergised g) initialHeadings
      where
        initialHeadings :: [Heading]
        initialHeadings =
          [ ((x, y), d)
            | x <- [0 .. maxX - 1],
              y <- [0 .. maxY - 1],
              x == 0 || y == 0 || x == maxX -1 || y == maxY - 1,
              d <- [U .. R]
          ]

        (maxX, maxY) = dims g
