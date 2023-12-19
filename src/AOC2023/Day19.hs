{-# LANGUAGE RecordWildCards #-}

module AOC2023.Day19
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, digits, fromParser)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Text.Parsec (char, letter, many, many1, newline, notFollowedBy, oneOf, optional, parse, string, try)
import Text.Parsec.String (Parser)
import Prelude hiding (GT, LT)

type Id = String

data Rating = X | M | A | S
  deriving (Show, Read)

data Op = LT | GT
  deriving (Show)

data Gate = Test Rating Op Int Id | Const Id
  deriving (Show)

type Workflow = (Id, [Gate])

data Ratings a = R {x :: a, m :: a, a :: a, s :: a}
  deriving (Show)

data Tree = Accepted | Rejected | Branch Rating Op Int Tree Tree
  deriving (Show)

constGate :: Parser Gate
constGate = Const <$> many1 letter

testGate :: Parser Gate
testGate = do
  rating <-
    try
      ( do
          c <- oneOf "xmas"
          notFollowedBy letter
          pure $ read [toUpper c]
      )

  op <-
    oneOf "<>" <&> \case
      '<' -> LT
      '>' -> GT
      _ -> error "Bad <>"

  ds <- digits
  char ':'
  wId <- many letter
  char ','
  pure $ Test rating op ds wId

workflow :: Parser Workflow
workflow = do
  wId <- many1 letter
  char '{'
  tGates <- many1 testGate
  cGate <- constGate
  char '}'
  newline
  pure (wId, tGates ++ [cGate])

ratings :: Parser (Ratings Int)
ratings = do
  string "{x="
  x <- digits
  string ",m="
  m <- digits
  string ",a="
  a <- digits
  string ",s="
  s <- digits
  char '}'
  optional newline
  pure $ R {..}

input :: Parser ([Workflow], [Ratings Int])
input = do
  ws <- many1 workflow
  newline
  rs <- many1 ratings
  pure (ws, rs)

buildTree :: M.Map Id [Gate] -> Id -> Tree
buildTree _ "A" = Accepted
buildTree _ "R" = Rejected
buildTree wMap wId = gatesToTree (wMap M.! wId)
  where
    gatesToTree [Const wId'] = buildTree wMap wId'
    gatesToTree (Test rating op n wId' : gs) =
      let left = buildTree wMap wId'
          right = gatesToTree gs
       in Branch rating op n left right
    gatesToTree _ = error "Bad tree"

part1 :: Solution
part1 = fromParser go . parse input ""
  where
    go (ws, ps) = sum $ map (foldTree tree) ps
      where
        tree = buildTree (M.fromList ws) "in"

        foldTree :: Tree -> Ratings Int -> Int
        foldTree Accepted p = sumRatings p
        foldTree Rejected _ = 0
        foldTree (Branch rating op n left right) p =
          foldTree (if passes then left else right) p
          where
            passes :: Bool
            passes =
              let f = case rating of
                    X -> x
                    M -> m
                    A -> a
                    S -> s
               in case op of
                    LT -> f p < n
                    GT -> f p > n

        sumRatings :: Ratings Int -> Int
        sumRatings (R {..}) = x + m + a + s

part2 :: Solution
part2 = fromParser go . parse input ""
  where
    go (ws, _) = foldTree tree (R minMax minMax minMax minMax)
      where
        tree = buildTree (M.fromList ws) "in"

        minMax = (1, 4000)

        foldTree :: Tree -> Ratings (Int, Int) -> Int
        foldTree Accepted rs = combinations rs
        foldTree Rejected _ = 0
        foldTree (Branch rating op n left right) rs@R {..} =
          foldTree left passRs + foldTree right failRs
          where
            (passRs, failRs) =
              let f = case op of
                    LT -> \(lower, upper) -> (lower, min upper (n - 1))
                    GT -> \(lower, upper) -> (max lower (n + 1), upper)
                  f' = case op of
                    LT -> \(lower, upper) -> (max lower n, upper)
                    GT -> \(lower, upper) -> (lower, min upper n)
               in case rating of
                    X -> (rs {x = f x}, rs {x = f' x})
                    M -> (rs {m = f m}, rs {m = f' m})
                    A -> (rs {a = f a}, rs {a = f' a})
                    S -> (rs {s = f s}, rs {s = f' s})

        combinations :: Ratings (Int, Int) -> Int
        combinations (R {..}) = product $ map size [x, m, a, s]
          where
            size (lower, upper) = upper - lower + 1
