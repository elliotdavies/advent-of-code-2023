#!/usr/bin/env bash

set -euxo pipefail

DAY_LOWER=$(echo "$DAY" | tr '[:upper:]' '[:lower:]')


cat <<EOF > "src/AOC2023/$DAY.hs"
module AOC2023.$DAY
  ( part1,
    part2,
  )
  where

import AOC2023.Lib (Solution)

part1 :: Solution
part1 = undefined

part2 :: Solution
part2 = undefined
EOF


cat <<EOF > "test/AOC2023/${DAY}Spec.hs"
module AOC2023.${DAY}Spec where

import AOC2023.$DAY
import Test.Hspec

spec :: Spec
spec =
  describe "$DAY" $ do
    it "Part 1 test" $ do
      input <- readFile "inputs/$DAY_LOWER/test.txt"
      part1 input \`shouldBe\` Right 0

    it "Part 1 real" $ do
      pending

    it "Part 2 test" $ do
      pending

    it "Part 2 real" $ do
      pending
EOF


mkdir "inputs/$DAY_LOWER"
touch "inputs/$DAY_LOWER/test.txt"
touch "inputs/$DAY_LOWER/real.txt"
