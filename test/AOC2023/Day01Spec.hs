module AOC2023.Day01Spec where

import Test.Hspec

import AOC2023.Day01

spec :: Spec
spec
  = describe "Day01" $ do
      it "Part 1" $ do
        part1 input1 `shouldBe` ""

      it "Part 2" $ do
        part2 input2 `shouldBe` ""
