{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day02
  ( part1,
    part2,
    cubes,
    handful,
    game,
    Color (..),
    Game (..),
  )
where

import AOC2023.Lib (Solution, digits, fromParser)
import Control.Applicative ((<|>))
import Text.Parsec (parse, sepBy)
import Text.Parsec.Char (space, string)
import Text.Parsec.String (Parser)

data Game = Game {gameId :: Int, red :: Int, green :: Int, blue :: Int}
  deriving (Eq, Show)

game :: Parser Game
game = do
  string "Game "
  gameId <- digits
  string ": "
  handfuls <- handful `sepBy` string "; "

  let maxReds = maxSeen Red handfuls
  let maxBlues = maxSeen Blue handfuls
  let maxGreens = maxSeen Green handfuls

  pure $ Game gameId maxReds maxGreens maxBlues
  where
    maxSeen :: Color -> [[(Color, Int)]] -> Int
    maxSeen color = maximum . map snd . filter ((== color) . fst) . concat

handful :: Parser [(Color, Int)]
handful = cubes `sepBy` string ", "

data Color = Red | Green | Blue
  deriving (Eq, Show)

readColor :: String -> Color
readColor = \case
  "red" -> Red
  "blue" -> Blue
  "green" -> Green
  other -> error $ "Unexpected color: " ++ other

cubes :: Parser (Color, Int)
cubes = do
  num <- digits
  space
  (,num) . readColor <$> (string "red" <|> string "green" <|> string "blue")

filterGames :: [Game] -> [Game]
filterGames =
  filter (\(Game {red, green, blue}) -> red <= 12 && green <= 13 && blue <= 14)

part1 :: Solution
part1 input =
  fromParser
    (sum . map gameId . filterGames)
    (mapM (parse game "") (filter (not . null) $ lines input))

part2 :: Solution
part2 input =
  fromParser
    (sum . map (\g -> red g * blue g * green g))
    (mapM (parse game "") (filter (not . null) $ lines input))
