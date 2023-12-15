module AOC2023.Day15
  ( part1,
    part2,
    hash,
  )
where

import AOC2023.Lib (Solution, digits, fromParser)
import Control.Applicative ((<|>))
import Data.Char (ord)
import qualified Data.IntMap as IM
import Text.Parsec (char, letter, many1, newline, optional, parse)
import Text.Parsec.String (Parser)

data Op = Upsert Int | Remove
  deriving (Eq, Show)

data Step = Step String Op

type LensConfig = IM.IntMap [(String, Int)]

stringify :: Step -> String
stringify (Step label (Upsert d)) = label ++ "=" ++ show d
stringify (Step label Remove) = label ++ "-"

remove :: Parser Op
remove = char '-' >> pure Remove

upsert :: Parser Op
upsert = char '=' >> Upsert <$> digits

input :: Parser [Step]
input = many1 $ do
  label <- many1 letter
  op <- remove <|> upsert
  optional $ char ','
  optional newline
  pure $ Step label op

hash :: String -> Int
hash = foldr go 0 . reverse
  where
    go :: Char -> Int -> Int
    go c acc = ((acc + ord c) * 17) `rem` 256

part1 :: Solution
part1 = fromParser go . parse input ""
  where
    go = sum . map (hash . stringify)

part2 :: Solution
part2 = fromParser go . parse input ""
  where
    go = focusPower . foldr buildMap IM.empty . reverse

    buildMap :: Step -> LensConfig -> LensConfig
    buildMap (Step label op) conf =
      let k = hash label
       in case op of
            Remove -> IM.adjust (filter (\(l, _) -> l /= label)) k conf
            Upsert d -> IM.insertWith merge k [(label, d)] conf
      where
        merge [(l, d)] old =
          if l `elem` map fst old
            then map (\pair -> if fst pair == l then (l, d) else pair) old
            else old ++ [(label, d)]
        merge _ _ = error "Bad merge"

    focusPower :: LensConfig -> Int
    focusPower = sum . map focusPower' . IM.toList
      where
        focusPower' :: (Int, [(String, Int)]) -> Int
        focusPower' (box, slots) =
          sum $ map (\((_, n), i) -> (box + 1) * n * i) $ zip slots [1 ..]
