module AOC2023.Day03Spec where

import AOC2023.Day03
import Test.Hspec

spec :: Spec
spec =
  describe "Day03" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day03/part1_test.txt"
      part1 input `shouldBe` 4361

    it "Part 1 real" $ do
      input <- readFile "inputs/day03/real.txt"
      pending

    it "Part 2 test" $ do
      -- input <- readFile "inputs/day03/part2_test.txt"
      pending

    it "Part 2 real" $ do
      -- input <- readFile "inputs/day03/real.txt"
      pending
