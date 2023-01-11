module Chapter01.LibSpec (spec) where

import Chapter01.Lib (Tree (..), map, relabel, (++))
import Test.Hspec
import Prelude hiding (map, (++))

spec :: Spec
spec = do
  describe "relabel" $ do
    it "tree" $ do
      let tree = Node (Node (Leaf 'x') (Leaf 'y')) (Leaf 'z')
      fst (relabel tree 1)
        `shouldBe` Node
          (Node (Leaf (1, 'x')) (Leaf (2, 'y')))
          (Leaf (3, 'z'))

  describe "concat" $ do
    it "two lists" $ do
      [1, 2] ++ [3, 4] `shouldBe` ([1, 2, 3, 4] :: [Int])

  describe "map" $ do
    it "over a list" $ do
      map (+ 1) [1, 2] `shouldBe` ([2, 3] :: [Int])
