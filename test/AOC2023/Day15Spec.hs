module AOC2023.Day15Spec where

import AOC2023.Day15
import Test.Hspec

spec :: Spec
spec =
  describe "Day15" $ do
    it "hash" $ do
      hash "HASH" `shouldBe` 52

    it "Part 1 test" $ do
      input <- readFile "inputs/day15/test.txt"
      part1 input `shouldBe` Right 1320

    it "Part 1 real" $ do
      input <- readFile "inputs/day15/real.txt"
      part1 input `shouldBe` Right 509784

    it "Part 2 test" $ do
      input <- readFile "inputs/day15/test.txt"
      part2 input `shouldBe` Right 145

    it "Part 2 real" $ do
      input <- readFile "inputs/day15/real.txt"
      part2 input `shouldBe` Right 230197
