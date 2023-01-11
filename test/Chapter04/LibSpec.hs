module Chapter04.LibSpec (spec) where

import Chapter04.Lib (filterM, replicateM, zipWithM)
import Test.Hspec

spec :: Spec
spec = do
  describe "monadic" $ do
    it "zipWith" $
      do
        zipWithM (\x y -> Just (x + y)) [1, 2] [3, 4]
        `shouldBe` Just ([4, 6] :: [Int])
    it "replicate" $
      do
        replicateM 2 $ Just 1
        `shouldBe` Just ([1, 1] :: [Int])
    it "filter" $
      do
        filterM (fmap (> 0) . Just) [0, 1]
        `shouldBe` Just ([1] :: [Int])
