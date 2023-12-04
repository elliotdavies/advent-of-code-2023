module AOC2023.Day04Spec where

import AOC2023.Day04
import Test.Hspec

spec :: Spec
spec =
  describe "Day04" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day04/test.txt"
      part1 input `shouldBe` Right 13

    it "Part 1 real" $ do
      input <- readFile "inputs/day04/real.txt"
      part1 input `shouldBe` Right 21821

    it "Part 2 test" $ do
      input <- readFile "inputs/day04/test.txt"
      part2 input `shouldBe` Right 30

    it "Part 2 real" $ do
      input <- readFile "inputs/day04/real.txt"
      part2 input `shouldBe` Right 5539496
