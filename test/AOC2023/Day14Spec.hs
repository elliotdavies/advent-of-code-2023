module AOC2023.Day14Spec where

import AOC2023.Day14
import Test.Hspec

spec :: Spec
spec =
  describe "Day14" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day14/test.txt"
      part1 input `shouldBe` Right 136

    it "Part 1 real" $ do
      input <- readFile "inputs/day14/real.txt"
      part1 input `shouldBe` Right 110407

    it "Part 2 test" $ do
      input <- readFile "inputs/day14/test.txt"
      part2 input `shouldBe` Right 64

    it "Part 2 real" $ do
      input <- readFile "inputs/day14/real.txt"
      part2 input `shouldBe` Right 87273
