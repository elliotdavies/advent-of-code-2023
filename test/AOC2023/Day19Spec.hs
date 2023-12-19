module AOC2023.Day19Spec where

import AOC2023.Day19
import Test.Hspec

spec :: Spec
spec =
  describe "Day19" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day19/test.txt"
      part1 input `shouldBe` Right 19114

    it "Part 1 real" $ do
      input <- readFile "inputs/day19/real.txt"
      part1 input `shouldBe` Right 487623

    it "Part 2 test" $ do
      input <- readFile "inputs/day19/test.txt"
      part2 input `shouldBe` Right 167409079868000

    it "Part 2 real" $ do
      input <- readFile "inputs/day19/real.txt"
      part2 input `shouldBe` Right 113550238315130
