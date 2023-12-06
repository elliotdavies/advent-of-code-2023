module AOC2023.Day05Spec where

import AOC2023.Day05
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec =
  describe "Day05" $ do
    it "parsers" $ do
      parse seedsParser "" "seeds: 79 14 55 13" `shouldBe` Right [79, 14, 55, 13]

      parse rangeParser "" "1 2 33" `shouldBe` Right (Range 1 2 33)

      parse mapParser "" "1 2 33\n44 5 6" `shouldBe` Right (Map [Range 1 2 33, Range 44 5 6])

    it "Part 1 test" $ do
      input <- readFile "inputs/day05/test.txt"
      part1 input `shouldBe` Right 35

    it "Part 1 real" $ do
      input <- readFile "inputs/day05/real.txt"
      part1 input `shouldBe` Right 510109797

    it "Part 2 test" $ do
      input <- readFile "inputs/day05/test.txt"
      part2 input `shouldBe` Right 46

    --  Skipped because it's a little slow
    xit "Part 2 real" $ do
      input <- readFile "inputs/day05/real.txt"
      part2 input `shouldBe` Right 9622622
