module AOC2023.Day13Spec where

import AOC2023.Day13
import Test.Hspec

spec :: Spec
spec =
  describe "Day13" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/day13/test.txt"
      part1 input `shouldBe` Right 405

    it "Part 1 test 2" $ do
      let input =
            "\
            \#.#.#.#..\n\
            \#.#.#.##.\n\
            \#.#.#.##.\n\
            \#.#.#.#..\n\
            \...#.....\n\
            \#..#.##..\n\
            \....#...#\n\
            \####....#\n\
            \#####....\n\
            \.###.#.##\n\
            \####...##\n\
            \.#.#...#.\n\
            \#....#.#."

      part1 input `shouldBe` Right 200

    it "Part 1 test 3" $ do
      let input =
            "\
            \.########..##\n\
            \.###..###.#.#\n\
            \###....###.##\n\
            \#...##...##.#\n\
            \.########.#..\n\
            \#...##..#####\n\
            \#.##..##.#...\n\
            \....##....#..\n\
            \....##....#.."

      part1 input `shouldBe` Right 800

    it "Part 1 real" $ do
      input <- readFile "inputs/day13/real.txt"
      part1 input `shouldBe` Right 33047

    it "Part 2 test" $ do
      input <- readFile "inputs/day13/test.txt"
      part2 input `shouldBe` Right 400

    it "part 2 test 2" $ do
      input <- readFile "inputs/day13/test2.txt"
      part2 input `shouldBe` Right 8

    it "Part 2 real" $ do
      input <- readFile "inputs/day13/real.txt"
      part2 input `shouldBe` Right 28806
