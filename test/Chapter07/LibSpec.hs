module Chapter07.LibSpec (spec) where

import Chapter07.Lib (pyts)
import qualified Control.Monad.Logic as L
import Test.Hspec

spec :: Spec
spec = do
  describe "logic monad" $ do
    it "finds pythagorean triplets" $ do
      let actual = L.observeAll $ pyts [1 .. 10]
      let expected =
            [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

      actual `shouldBe` expected
