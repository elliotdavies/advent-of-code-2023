module AOC2023.Day06Spec where

import AOC2023.Day06
import Test.Hspec

spec :: Spec
spec =
  describe "Day06" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day06/test.txt"
      part1 input `shouldBe` Right 288

    it "Part 1 real" $ do
      input <- readFile "inputs/day06/real.txt"
      part1 input `shouldBe` Right 1710720

    it "Part 2 test" $ do
      input <- readFile "inputs/day06/test.txt"
      part2 input `shouldBe` Right 71503

    it "Part 2 real" $ do
      input <- readFile "inputs/day06/real.txt"
      part2 input `shouldBe` Right 35349468
