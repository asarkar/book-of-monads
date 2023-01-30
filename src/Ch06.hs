{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ch06 () where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State s1) = State $ \x -> let (a, s') = s1 x in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State fab <*> State sas = State $ \s0 ->
    let (f, s1) = fab s0
        (x, s2) = sas s1
     in (f x, s2)

-- Exercise 6.1. Write the `Monad` instance for this variation of `State s`.

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  State sas >>= fasbs =
    State $ \s0 -> let (a, s1) = sas s0 in runState (fasbs a) s1

{-
Exercise 6.2. Define the function `modify` in terms of `get` and `put`.

modify :: (s -> s) -> State s ()
modify f = do
  i <- get
  put (f i)

type Reader r a = r -> a
instance Monad (Reader r) where
  return = flip const -- also, const id
  {-
  x env = a
  f a = m b = r -> b
  f a env = b
  -}
  x >> f = \env -> f (x env) env
-}

{-
Exercise 6.3. Consider the following type that is a contravariant functor.
Write the corresponding instance.

newtype Return r a = Return (a -> r)

instance Contravariant (Return r) where
  -- contramap :: (a -> b) -> f b -> f a
  -- First turn a into b, and then apply b -> r
  contramap f (Return rb) = Return $ rb . f
-}
