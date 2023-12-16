module AOC2023.Day16Spec where

import AOC2023.Day16
import Test.Hspec

spec :: Spec
spec =
  describe "Day16" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day16/test.txt"
      part1 input `shouldBe` Right 46

    xit "Part 1 real" $ do
      input <- readFile "inputs/day16/real.txt"
      part1 input `shouldBe` Right 0

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
