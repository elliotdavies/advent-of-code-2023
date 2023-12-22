module AOC2023.Day21Spec where

import AOC2023.Day21
import Test.Hspec

spec :: Spec
spec =
  describe "Day21" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day21/test.txt"
      part1 6 input `shouldBe` Right 16

    it "Part 1 real" $ do
      input <- readFile "inputs/day21/real.txt"
      part1 64 input `shouldBe` Right 3562

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
