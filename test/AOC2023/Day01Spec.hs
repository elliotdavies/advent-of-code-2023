module AOC2023.Day01Spec where

import AOC2023.Day01
import Test.Hspec

spec :: Spec
spec =
  describe "Day01" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day01/part1_test.txt"
      part1 input `shouldBe` Right 142

    it "Part 1 real" $ do
      input <- readFile "inputs/day01/real.txt"
      part1 input `shouldBe` Right 53080

    it "Part 2 test" $ do
      input <- readFile "inputs/day01/part2_test.txt"
      part2 input `shouldBe` Right 281

    it "Part 2 real" $ do
      input <- readFile "inputs/day01/real.txt"
      part2 input `shouldBe` Right 53268
