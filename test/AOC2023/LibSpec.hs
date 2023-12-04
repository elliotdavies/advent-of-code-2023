module AOC2023.LibSpec where

import AOC2023.Lib
import qualified Data.Vector as V
import Test.Hspec

-- import AOC2023.Lib

spec :: Spec
spec = describe "Lib" $ do
  describe "digitsToInt" $ do
    it "works" $ do
      digitsToInt [1] `shouldBe` 1
      digitsToInt [1, 7] `shouldBe` 17
      digitsToInt [3, 16, 457] `shouldBe` 316457

  describe "(!?!?)" $ do
    it "works" $ do
      let vec =
            V.fromList
              [ V.fromList ['a', 'b', 'c'],
                V.fromList ['d', 'e', 'f'],
                V.fromList ['g', 'h', 'i']
              ]

      vec !?!? (0, 2) `shouldBe` Just 'g'
      vec !?!? (1, 1) `shouldBe` Just 'e'
      vec !?!? (2, 1) `shouldBe` Just 'f'

  describe "surroundingCoords" $ do
    it "works" $ do
      surroundingCoords (5, 10)
        `shouldBe` [ (4, 9),
                     (5, 9),
                     (6, 9),
                     (4, 10),
                     (6, 10),
                     (4, 11),
                     (5, 11),
                     (6, 11)
                   ]
