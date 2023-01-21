{-# LANGUAGE TupleSections #-}

module Chapter08.LibSpec (spec) where

import Chapter08.Lib (addName)
import qualified Data.List as L
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  describe "transactional variables" $ do
    prop "insertion" $
      forAll pickName $
        \(xs, x) -> ioProperty $ do
          ys <- addName xs x
          return $ verify xs x ys

    it "insertion - monadic" $
      forAll pickName $
        \(xs, x) -> monadicIO $ do
          ys <- run $ addName xs x
          assert $ verify xs x ys

verify :: [String] -> String -> [(Int, String)] -> Bool
verify xs x ys
  | x `elem` xs = ix == ns && names' == zs
  | otherwise =
      L.uncons ix == Just (n, ns)
        && L.uncons names' == Just (x, zs)
  where
    (ix, names') = unzip ys
    n = length xs
    ns = [n - 1, n - 2 .. 0]
    zs = reverse xs

name :: Gen String
name = getPrintableString <$> arbitrary

names :: Gen [String]
names = resize 50 (listOf name)

pickName :: Gen ([String], String)
pickName = do
  xs <- names
  ys <- names
  fmap (xs,) $ elements $ xs ++ ys
