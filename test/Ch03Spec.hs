module Ch03Spec (spec) where

import Ch03 (ZipList (..), ap, mapZ)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "applies to" $ do
    prop "Maybe" prop_Ap

  describe "maps over" $ do
    prop "ZipList" $
      forAll genZ $
        \(f, xs) ->
          let f' = applyFun f
           in mapZ f' (ZipList xs) == ZipList (map f' xs)

prop_Ap :: Maybe (Fun Int Int) -> Maybe Int -> Bool
prop_Ap f x = ap f' x == (f' <*> x)
  where
    f' = fmap applyFun f

genZ :: Gen (Fun Int Int, [Int])
genZ = do
  f <- arbitrary
  xs <- vectorOf 100 $ chooseInt (0, 1000)
  return (f, xs)
