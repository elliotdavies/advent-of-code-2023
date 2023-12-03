module AOC2023.Day02Spec where

import AOC2023.Day02
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec =
  describe "Day02" $ do
    it "parsers" $ do
      parse cubes "" "3 blue" `shouldBe` Right (Blue, 3)
      parse cubes "" "12 red" `shouldBe` Right (Red, 12)
      parse cubes "" "1 green" `shouldBe` Right (Green, 1)

      parse handful "" "1 blue, 2 green" `shouldBe` Right [(Blue, 1), (Green, 2)]
      parse handful "" "1 green, 2 red;" `shouldBe` Right [(Green, 1), (Red, 2)]

      parse game "" "Game 23: 1 green, 2 red; 3 blue, 14 green" `shouldBe` Right (Game 23 2 14 3)

    it "Part 1 test" $ do
      input <- readFile "inputs/day02/part1_test.txt"
      part1 input `shouldBe` 8

    it "Part 1 real" $ do
      input <- readFile "inputs/day02/real.txt"
      part1 input `shouldBe` 2505

    it "Part 2 test" $ do
      -- input <- readFile "inputs/day02/part2_test.txt"
      pending

    it "Part 2 real" $ do
      -- input <- readFile "inputs/day02/real.txt"
      pending
