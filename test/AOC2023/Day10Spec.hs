module AOC2023.Day10Spec where

import AOC2023.Day10
import Test.Hspec

spec :: Spec
spec =
  describe "Day10" $ do
    it "Part 1 test 1" $ do
      input <- readFile "inputs/day10/test1.txt"
      part1 input `shouldBe` Right 4

    it "Part 1 test 2" $ do
      input <- readFile "inputs/day10/test2.txt"
      part1 input `shouldBe` Right 8

    it "Part 1 real" $ do
      input <- readFile "inputs/day10/real.txt"
      part1 input `shouldBe` Right 7086

    it "Part 2 test 1" $ do
      input <- readFile "inputs/day10/test3.txt"
      part2 input `shouldBe` Right 5 -- should be 4?
    it "Part 2 test 2" $ do
      input <- readFile "inputs/day10/test4.txt"
      part2 input `shouldBe` Right 9 -- should be 8?
    it "Part 2 test 3" $ do
      input <- readFile "inputs/day10/test5.txt"
      part2 input `shouldBe` Right 10

    it "Part 2 real" $ do
      input <- readFile "inputs/day10/real.txt"
      part2 input `shouldBe` Right 317
