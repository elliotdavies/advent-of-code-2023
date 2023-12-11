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

  describe "transpose" $ do
    it "works" $ do
      let vec =
            V.fromList
              [ V.fromList ['a', 'b', 'c'],
                V.fromList ['d', 'e', 'f'],
                V.fromList ['g', 'h', 'i']
              ]

      transpose vec
        `shouldBe` V.fromList
          [ V.fromList ['a', 'd', 'g'],
            V.fromList ['b', 'e', 'h'],
            V.fromList ['c', 'f', 'i']
          ]

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

  describe "findCoords" $ do
    it "works" $ do
      let vec =
            V.fromList
              [ V.fromList ['a', 'b', 'c'],
                V.fromList ['d', 'e', 'f'],
                V.fromList ['g', 'h', 'i']
              ]

      findCoords ('b' ==) vec `shouldBe` Just (1, 0)
      findCoords ('f' ==) vec `shouldBe` Just (2, 1)
      findCoords ('g' ==) vec `shouldBe` Just (0, 2)

  describe "findAllCoords" $ do
    it "works" $ do
      let vec :: V.Vector (V.Vector Int) =
            V.fromList
              [ V.fromList [1, 4, 8],
                V.fromList [6, 2, 7],
                V.fromList [9, 5, 3]
              ]

      findAllCoords (>= 7) vec `shouldBe` [(0, 2), (2, 1), (2, 0)]

  describe "manhattanDist" $ do
    it "works" $ do
      manhattanDist (2, 3) (9, 7) `shouldBe` 11
