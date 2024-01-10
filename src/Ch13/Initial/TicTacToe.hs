{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ch13.Initial.TicTacToe where

import Ch13.TicTacToe

-- import Control.Monad ((>=>))

data TicTacToe a
  = Info Position (Maybe Player -> TicTacToe a)
  | Take Position (Result -> TicTacToe a)
  | Done a

{-
Exercise 13.5. Prove that correspondence by writing two functions,
from :: (() -> a) -> a and to :: a -> (() -> a).
-}

from :: (() -> a) -> a
from f = f ()

-- from = ($ ()) -- for point-free implementation

to :: a -> (() -> a)
to = const

-- A Monad instance requires an Applicative instance.

-- instance Monad TicTacToe where
--   (>>=) :: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b
--   Done x   >>= f = f x
--   Info p g >>= f = Info p (g >=> f)
--   Take p g >>= f = Take p (g >=> f)

{-
Exercise 13.6. Write the Functor instance for TicTacToe.
If you feel brave, try Applicative.
-}

instance Functor TicTacToe where
  fmap :: (a -> b) -> TicTacToe a -> TicTacToe b
  fmap f (Done x) = Done (f x)
  fmap f (Info pos g) = Info pos (fmap f . g)
  fmap f (Take pos g) = Take pos (fmap f . g)

-- The applicative instance doesn't make sense
-- since there's no logical way to combine two positions.
--
-- instance Applicative TicTacToe where
--   pure :: a -> TicTacToe a
--   pure = Done
--
--   (<*>) :: TicTacToe (a -> b) -> TicTacToe a -> TicTacToe b
--   Done f     <*> Done x      = Done $ f x
--   Info pos f <*> Info pos' g = Info
--     (pos + pos')
--     (\pl ->
--       let f' = f pl
--           g' = g pl
--       in  f' <*> g'
--     )
--   Take pos f <*> Take pos' x = Take
--     (pos + pos')
--     (\y ->
--       let f' = f y
--           x' = x y
--       in  f' <*> x'
--     )
