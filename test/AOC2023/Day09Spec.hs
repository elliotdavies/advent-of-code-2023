module AOC2023.Day09Spec where

import AOC2023.Day09
import Test.Hspec

spec :: Spec
spec =
  describe "Day09" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day09/test.txt"
      part1 input `shouldBe` Right 114

    it "Part 1 real" $ do
      input <- readFile "inputs/day09/real.txt"
      part1 input `shouldBe` Right 1772145754

    it "Part 2 test" $ do
      input <- readFile "inputs/day09/test.txt"
      part2 input `shouldBe` Right 2

    it "Part 2 real" $ do
      input <- readFile "inputs/day09/real.txt"
      part2 input `shouldBe` Right 867
