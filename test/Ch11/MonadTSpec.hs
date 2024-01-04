-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html
{-# LANGUAGE ExplicitForAll #-}
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ch11.MonadTSpec (spec) where

import Ch11.MonadT (ExT (..), MbT (..), StT (..), runStT)
import Data.Function as F
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity.Applicative
import Test.Validity.Eq
import Test.Validity.Functor
import Test.Validity.Monad
import Test.Validity.Show

type ESM = ExT String Maybe

type ESMI = ESM Int

spec :: Spec
spec = do
  eqSpecOnArbitrary @ESMI
  showReadSpecOnArbitrary @ESMI
  functorSpecOnArbitrary @ESM
  applicativeSpecOnArbitrary @ESM
  monadSpecOnArbitrary @ESM

  monadSpecOnArbitrary @(MbT [])

  describe "Monad (StT Int [])" $ do
    describe "satisfies Monad laws" $ do
      -- the types are in the same order as in `forall`
      prop "right identity law" (prop_monadRightId @Int @Int @[])
      prop "left identity law" (prop_monadLeftId @Int @Int @Int @[])
      prop "associative law" (prop_monadAssoc @Int @Int @Int @Int @[])

instance (Arbitrary e, Arbitrary1 m, Arbitrary a) => Arbitrary (ExT e m a) where
  arbitrary = ExT <$> arbitrary1

instance (Arbitrary1 m, Arbitrary a) => Arbitrary (MbT m a) where
  arbitrary = MbT <$> arbitrary1

{- HLINT ignore -}

{-
the types in `forall` are specified in the order of dependency.
since `m` needs `a` and `s`, those appear before `m` in the list.
-}

-- (x >>= return) == x
prop_monadRightId ::
  forall a s m.
  (Monad m, Eq (m (a, s)), Show (m (a, s))) =>
  s ->
  Fun s (m (a, s)) ->
  Property
prop_monadRightId s f = ((===) `F.on` go) (m >>= return) m
  where
    m = StT $ applyFun f
    go st = runStT st s

-- (return x >>= f) == (f x)
prop_monadLeftId ::
  forall a b s m.
  (Monad m, Eq (m (b, s)), Show (m (b, s))) =>
  a ->
  s ->
  Fun (a, s) (m (b, s)) ->
  Property
prop_monadLeftId a s f = ((===) `F.on` go) (return a >>= h) m
  where
    g = applyFun2 f
    m = StT $ g a
    h = StT . g
    go st = runStT st s

-- ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))
prop_monadAssoc ::
  forall a b c s m.
  (Monad m, Eq (m (c, s)), Show (m (c, s))) =>
  s ->
  Fun s (m (a, s)) ->
  Fun (a, s) (m (b, s)) ->
  Fun (b, s) (m (c, s)) ->
  Property
prop_monadAssoc s h f g =
  ((===) `F.on` go)
    ((m >>= f') >>= g')
    (m >>= (\x -> f' x >>= g'))
  where
    m = StT $ applyFun h
    f' = StT . applyFun2 f
    g' = StT . applyFun2 g
    go st = runStT st s
