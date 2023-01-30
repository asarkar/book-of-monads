module Ch00Spec (spec) where

import Ch00 (eqList, eqTuple, insertList, isEmptyList, notEq)
import qualified Control.Monad as M
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "checks tuple" $ do
    prop "equality" $
      \x y ->
        eqTuple (x :: (Bool, Bool)) y == (x == y)
          && notEq x y == (x /= y)

  describe "checks list" $ do
    prop "equality" $
      forAll genLists $
        \(xs, ys) -> eqList xs ys == (xs == ys)

    prop "insertion" $
      forAll (M.liftM2 (,) genBool genList) $
        \(x, xs) -> insertList x xs == x : xs

    prop "emptiness" $
      forAll genList $
        \xs -> isEmptyList xs == null xs

genBool :: Gen Bool
genBool = arbitrary

genList :: Gen [Bool]
genList = vectorOf 100 genBool

genLists :: Gen ([Bool], [Bool])
genLists = do
  xs <- genList
  ys <- shuffle xs
  return (xs, ys)
