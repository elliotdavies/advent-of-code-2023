module AOC2023.Day16Spec where

import AOC2023.Day16
import Test.Hspec

spec :: Spec
spec =
  describe "Day16" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day16/test.txt"
      part1 input `shouldBe` Right 46

    it "Part 1 real" $ do
      input <- readFile "inputs/day16/real.txt"
      part1 input `shouldBe` Right 7788

    it "Part 2 test" $ do
      input <- readFile "inputs/day16/test.txt"
      part2 input `shouldBe` Right 51

    -- Skipped because it's a bit slow
    xit "Part 2 real" $ do
      input <- readFile "inputs/day16/real.txt"
      part2 input `shouldBe` Right 7987
