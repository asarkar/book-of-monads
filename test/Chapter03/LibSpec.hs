module Chapter03.LibSpec (spec) where

import Chapter03.Lib (ZipList (..), ap, mapZ)
import Test.Hspec

spec :: Spec
spec = do
  describe "apply to" $ do
    it "Maybe" $ do
      ap (Just (+ 1)) (Just 1) `shouldBe` Just (2 :: Int)
  describe "map over" $ do
    it "ZipList" $ do
      mapZ (+ 1) (ZipList [1, 2]) `shouldBe` ZipList ([2, 3] :: [Int])
