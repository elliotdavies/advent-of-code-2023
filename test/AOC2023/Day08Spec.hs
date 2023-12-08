module AOC2023.Day08Spec where

import AOC2023.Day08
import Test.Hspec

spec :: Spec
spec =
  describe "Day08" $ do
    it "Part 1 test 1" $ do
      input <- readFile "inputs/day08/test1.txt"
      part1 input `shouldBe` Right 2

    it "Part 1 test 2" $ do
      input <- readFile "inputs/day08/test2.txt"
      part1 input `shouldBe` Right 6

    it "Part 1 real" $ do
      input <- readFile "inputs/day08/real.txt"
      part1 input `shouldBe` Right 13019

    it "Part 2 test" $ do
      input <- readFile "inputs/day08/test3.txt"
      part2 input `shouldBe` Right 6

    it "Part 2 real" $ do
      input <- readFile "inputs/day08/real.txt"
      part2 input `shouldBe` Right 13524038372771
