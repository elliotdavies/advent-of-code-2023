module AOC2023.Day16
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Coords, Dir (..), Solution, fromParser, travel, (!?!?))
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
  | SplitterH -- -

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

followBeam :: Grid -> Coords -> Dir -> S.Set (Dir, Coords)
followBeam g initCoords initDir = go initCoords initDir S.empty
  where
    go :: Coords -> Dir -> S.Set (Dir, Coords) -> S.Set (Dir, Coords)
    go cs d s
      | S.member (d, cs) s = s
      | otherwise =
        case g !?!? cs of
          Just Empty -> follow d
          Just MirrorF ->
            follow $ case d of
              U -> R
              R -> U
              D -> L
              L -> D
          Just MirrorB ->
            follow $ case d of
              U -> L
              L -> U
              D -> R
              R -> D
          Just SplitterV ->
            if d == L || d == R
              then S.union (follow U) (follow D)
              else follow d
          Just SplitterH ->
            if d == U || d == D
              then S.union (follow L) (follow R)
              else follow d
          Nothing -> s
      where
        follow d' = go (travel d' cs) d' (S.insert (d, cs) s)

part1 :: Solution
part1 = fromParser go . parse input ""
  where
    go g = S.size $ S.map snd $ followBeam g (0, 0) R

part2 :: Solution
part2 = undefined
