{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Freer.Program where

data Program instr a where
  Done :: a -> Program instr a
  Bind :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr :: instr a -> Program instr a

instance Functor (Program instr) where
  fmap :: (a -> b) -> Program instr a -> Program instr b
  fmap = (=<<) . (pure .)

instance Applicative (Program instr) where
  pure :: a -> Program instr a
  pure = Done

  (<*>) :: Program instr (a -> b) -> Program instr a -> Program instr b
  (<*>) = (. flip fmap) . (>>=)

{-
Exercise 13.14. Write the Monad instance for Program instr.
Note that this instance does not depend on which set of
instructions, instr, you work on.
Hint: as in the case of operational style monads, this instance
is obtained by using some of the constructors in Program instr.
-}
instance Monad (Program instr) where
  (>>=) :: Program instr a -> (a -> Program instr b) -> Program instr b
  (>>=) = Bind
