module AOC2023.Day20Spec where

import AOC2023.Day20
import Test.Hspec

spec :: Spec
spec =
  describe "Day20" $ do
    it "Part 1 test 1" $ do
      input <- readFile "inputs/day20/test1.txt"
      part1 input `shouldBe` Right 32000000

    it "Part 1 test 2" $ do
      input <- readFile "inputs/day20/test2.txt"
      part1 input `shouldBe` Right 11687500

    it "Part 1 real" $ do
      input <- readFile "inputs/day20/real.txt"
      part1 input `shouldBe` Right 747304011

    it "Part 2 real" $ do
      input <- readFile "inputs/day20/real.txt"
      part2 input `shouldBe` Right 0
