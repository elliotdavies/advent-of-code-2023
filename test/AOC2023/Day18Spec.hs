module AOC2023.Day18Spec where

import AOC2023.Day18
import Test.Hspec

spec :: Spec
spec =
  describe "Day18" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day18/test.txt"
      part1 input `shouldBe` Right 62

    it "Part 1 real" $ do
      input <- readFile "inputs/day18/real.txt"
      part1 input `shouldBe` Right 48795

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
