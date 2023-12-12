module AOC2023.Day12Spec where

import AOC2023.Day12
import Test.Hspec

spec :: Spec
spec =
  describe "Day12" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day12/test.txt"
      part1 input `shouldBe` Right 21

    it "Part 1 real" $ do
      input <- readFile "inputs/day12/real.txt"
      part1 input `shouldBe` Right 6949

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
