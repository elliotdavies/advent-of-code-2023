module AOC2023.Day03Spec where

import AOC2023.Day03
import Test.Hspec

spec :: Spec
spec =
  describe "Day03" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day03/test.txt"
      part1 input `shouldBe` Right 4361

    it "Part 1 real" $ do
      input <- readFile "inputs/day03/real.txt"
      part1 input `shouldBe` Right 553825

    it "Part 2 test" $ do
      input <- readFile "inputs/day03/test.txt"
      part2 input `shouldBe` Right 467835

    it "Part 2 real" $ do
      input <- readFile "inputs/day03/real.txt"
      part2 input `shouldBe` Right 93994191
