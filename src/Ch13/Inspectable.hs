{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Ch13.Inspectable where

data LastOp = Return | LastPop | LastPush Integer

class MonadIStack m where
  pop :: m Integer
  push :: Integer -> m ()

newtype WithContext c m a = C
  { unC :: c -> m (a, c)
  }

-- Exercise 13.19 -- Implement Functor and Applicative for `WithContext LastOp m`.
instance (Monad m) => Functor (WithContext LastOp m) where
  fmap :: (a -> b) -> WithContext LastOp m a -> WithContext LastOp m b
  fmap f x = C $ \context -> do
    (x', context') <- unC x context
    pure (f x', context')

instance (Monad m) => Applicative (WithContext LastOp m) where
  pure :: a -> WithContext LastOp m a
  pure x = C $ \_ -> return (x, Return)

  (<*>) :: WithContext LastOp m (a -> b) -> WithContext LastOp m a -> WithContext LastOp m b
  f <*> x = C $ \context -> do
    (f', context') <- unC f context
    (x', context_) <- unC x context'
    pure (f' x', context_)

instance (Monad m) => Monad (WithContext LastOp m) where
  (>>=) :: WithContext LastOp m a -> (a -> WithContext LastOp m b) -> WithContext LastOp m b
  C x >>= f = C $ \context -> do
    (x', context') <- x context
    unC (f x') context'

instance (Monad m, MonadIStack m) => MonadIStack (WithContext LastOp m) where
  pop :: WithContext LastOp m Integer
  pop = C $ \case
    LastPush n -> return (n, Return)
    _ -> (,LastPop) <$> pop

  push :: Integer -> WithContext LastOp m ()
  push v = C $ \_ -> (,LastPush v) <$> push v

optimize :: (Monad m) => WithContext LastOp m a -> m a
optimize p = fst <$> unC p Return

{-
Exercise 13.20. Convince yourself that the previous code works.
Here, we are using the monadic computation p with the `typeWithContext LastOp m`.
Since the result is another computation that is parametric over MonadIStack
coupled with the final state, we can just ignore the second part.
-}
