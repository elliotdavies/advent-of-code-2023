module AOC2023.Day05Spec where

import AOC2023.Day05
import Test.Hspec

spec :: Spec
spec =
  describe "Day05" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day05/test.txt"
      part1 input `shouldBe` Right 35

    it "Part 1 real" $ do
      pending

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
