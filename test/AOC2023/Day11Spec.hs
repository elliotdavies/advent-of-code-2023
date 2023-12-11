module AOC2023.Day11Spec where

import AOC2023.Day11
import Test.Hspec

spec :: Spec
spec =
  describe "Day11" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day11/test.txt"
      part1 input `shouldBe` Right 374

    it "Part 1 real" $ do
      input <- readFile "inputs/day11/real.txt"
      part1 input `shouldBe` Right 9609130

    it "Part 2 test 10x" $ do
      input <- readFile "inputs/day11/test.txt"
      part2 10 input `shouldBe` Right 1030

    it "Part 2 test 100" $ do
      input <- readFile "inputs/day11/test.txt"
      part2 100 input `shouldBe` Right 8410

    it "Part 2 real" $ do
      input <- readFile "inputs/day11/real.txt"
      part2 1_000_000 input `shouldBe` Right 702152204842
