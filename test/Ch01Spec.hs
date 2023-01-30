module Ch01Spec (spec) where

import Ch01 (Tree (..), map, relabel, (++))
import qualified Control.Monad as M
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (map, (++))
import qualified Prelude as P

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
      forAll (M.liftM2 (,) genList genList) $
        \(xs, ys) -> xs ++ ys == (xs P.++ ys)

    prop "mapping" $
      forAll genList $
        \xs -> map (+ 1) xs == P.map (+ 1) xs

genList :: Gen [Int]
genList = vectorOf 100 $ chooseInt (0, 1000)
