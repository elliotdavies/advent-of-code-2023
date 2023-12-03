module AOC2023.Day02Spec where

import AOC2023.Day02
import Test.Hspec

spec :: Spec
spec =
  describe "Day02" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day02/part1_test.txt"
      part1 input `shouldBe` 8

    it "Part 1 real" $ do
      -- input <- readFile "inputs/day02/real.txt"
      pending

    it "Part 2 test" $ do
      -- input <- readFile "inputs/day02/part2_test.txt"
      pending

    it "Part 2 real" $ do
      -- input <- readFile "inputs/day02/real.txt"
      pending
