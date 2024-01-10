{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Freer.Freer where

import Ch13.Freer.Program (Program (..))
import Control.Monad ((<=<), (>=>))

data Freer instr a where
  Pure :: a -> Freer instr a
  Impure :: instr a -> (a -> Freer instr b) -> Freer instr b

{-
Exercise 13.15. Write the Functor and Applicative instances for Freer instr.
-}
instance Functor (Freer instr) where
  fmap :: (a -> b) -> Freer instr a -> Freer instr b
  fmap f (Pure x) = Pure $ f x
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (Freer instr) where
  pure :: a -> Freer instr a
  pure = Pure

  (<*>) :: Freer instr (a -> b) -> Freer instr a -> Freer instr b
  Pure f <*> Pure x = Pure $ f x
  Impure x k <*> f = Impure x (\a -> k a <*> f)
  Pure f <*> Impure x k = Impure x (fmap f . k)

instance Monad (Freer instr) where
  (>>=) :: Freer instr a -> (a -> Freer instr b) -> Freer instr b
  Pure x >>= f = f x
  Impure x k >>= f = Impure x (f <=< k)

{-
Exercise 13.16. Write functions twoToThree and threeToTwo that convert
between the freer monads with two and three constructors.
Hint: for the latter, it is useful to follow the same shape of recursion as step.
-}
twoToThree :: Freer instr a -> Program instr a
twoToThree (Pure x) = Done x
twoToThree (Impure x f) = Bind (Instr x) $ twoToThree . f

threeToTwo :: Program instr a -> Freer instr a
threeToTwo (Done x) = Pure x
threeToTwo (Instr ia) = Impure ia Pure
threeToTwo (Bind (Done x) f) = threeToTwo $ f x
threeToTwo (Bind (Instr ia) f) = Impure ia (threeToTwo . f)
-- f' :: b -> Program instr c
-- f  :: a -> Program instr b
-- x  :: Program instr a
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
threeToTwo (Bind (Bind x f') f) = threeToTwo $ x >>= (f' >=> f)
