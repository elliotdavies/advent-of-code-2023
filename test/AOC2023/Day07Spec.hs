module AOC2023.Day07Spec where

import AOC2023.Day07
import Test.Hspec

spec :: Spec
spec =
  describe "Day07" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day07/test.txt"
      part1 input `shouldBe` Right 0

    it "Part 1 real" $ do
      pending

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
