module Ch04Spec (spec) where

import Ch04 (filterM, replicateM, zipWithM)
import qualified Control.Monad as M
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "monadic" $ do
    prop "zipWith" $
      forAll genZ $
        \(f, xs, ys) ->
          let f' = applyFun2 f
           in zipWithM f' xs ys
                == M.zipWithM f' xs ys

    prop "replicate" $
      \x y ->
        replicateM x (y :: Maybe Int) == M.replicateM x y

    prop "filter" $
      forAll genF $
        \(f, xs) ->
          let f' = applyFun f
           in filterM f' xs
                == M.filterM f' xs

genList :: Gen [Int]
genList = vectorOf 100 $ chooseInt (0, 1000)

genZ :: Gen (Fun (Int, Int) (Maybe Int), [Int], [Int])
genZ = (,,) <$> arbitrary <*> genList <*> genList

genF :: Gen (Fun Int (Maybe Bool), [Int])
genF = (,) <$> arbitrary <*> genList
