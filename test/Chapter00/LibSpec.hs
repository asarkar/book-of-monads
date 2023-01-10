module Chapter00.LibSpec (spec) where

import Chapter00.Lib (eqList, eqTuple)
import Test.Hspec

spec :: Spec
spec = do
  describe "equate" $ do
    it "tuples" $ do
      let x = (True, True)
      let y = (True, True)
      eqTuple x y `shouldBe` True
    it "lists" $ do
      let x = [True, True]
      let y = [True, True]
      eqList x y `shouldBe` True
