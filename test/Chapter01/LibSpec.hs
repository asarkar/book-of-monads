module Chapter01.LibSpec (spec) where

import Chapter01.Lib (Tree (..), map, relabel, (++))
import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (map, (++))
import qualified Prelude

spec :: Spec
spec = do
  describe "relabels" $ do
    it "tree" $ do
      let tree = Node (Node (Leaf 'x') (Leaf 'y')) (Leaf 'z')
      fst (relabel tree 1)
        `shouldBe` Node
          (Node (Leaf (1, 'x')) (Leaf (2, 'y')))
          (Leaf (3, 'z'))

  describe "checks lists" $ do
    prop "concatenation" $
      forAll (liftM2 (,) genList genList) $
        \(xs, ys) -> xs ++ ys == (xs Prelude.++ ys)

    prop "mapping" $
      forAll genList $
        \xs -> map (+ 1) xs == Prelude.map (+ 1) xs

genList :: Gen [Int]
genList = resize 100 (listOf $ chooseInt (0, 1000))
