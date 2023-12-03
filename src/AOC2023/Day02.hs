{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AOC2023.Day02
  ( Input,
    Output,
    part1,
    part2,
    cubes,
    handful,
    game,
    Color (..),
    Game (..),
  )
where

import Control.Applicative ((<|>))
import Debug.Trace (traceShow)
import Text.Parsec (many1, parse, sepBy)
import Text.Parsec.Char (digit, space, string)
import Text.Parsec.String (Parser)

type Input = String

type Output = Int

data Game = Game {gameId :: Int, red :: Int, green :: Int, blue :: Int}
  deriving (Eq, Show)

game :: Parser Game
game = do
  string "Game "
  gameId <- read <$> many1 digit
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
  num <- read <$> many1 digit
  space
  (,num) . readColor <$> (string "red" <|> string "green" <|> string "blue")

filterGames :: [Game] -> [Game]
filterGames = filter (\(Game {red, green, blue}) -> red <= 12 && green <= 13 && blue <= 14)

part1 :: Input -> Output
part1 input =
  let results = sequence $ map (parse game "") (filter (not . null) $ lines input)
   in case results of
        Right gs -> sum $ map gameId $ filterGames gs
        Left err -> length $ traceShow err (show err)

part2 :: Input -> Output
part2 input =
  let results = sequence $ map (parse game "") (filter (not . null) $ lines input)
   in case results of
        Right gs -> sum $ map (\g -> red g * blue g * green g) gs
        Left err -> length $ traceShow err (show err)
