module Chapter00.LibSpec (spec) where

import Chapter00.Lib (eqList, eqTuple, insertList, isEmptyList, notEq)
import Test.Hspec
import Prelude hiding (Eq (..))

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
  describe "does not equate" $ do
    it "tuples" $ do
      let x = (True, False)
      let y = (True, True)
      eqTuple x y `shouldBe` False
    it "lists" $ do
      let x = [True, False]
      let y = [True, True]
      eqList x y `shouldBe` False
    it "boolean" $ do
      let x = (True, True)
      let y = (True, True)
      notEq x y `shouldBe` False
  describe "lists" $ do
    it "empty" $ do
      isEmptyList ([] :: [Bool]) `shouldBe` True
      isEmptyList [True] `shouldBe` False
    it "insert" $ do
      insertList True [] `shouldBe` [True]
      insertList True [True] `shouldBe` [True, True]
