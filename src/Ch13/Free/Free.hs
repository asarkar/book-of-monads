{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Ch13.Free.Free where

data Free f a
  = Free (f (Free f a))
  | Pure a

instance (Functor f) => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance (Functor f) => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure

  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure f <*> Pure x = Pure (f x)
  Pure f <*> Free x = Free (fmap (fmap f) x)
  Free f <*> x = Free (fmap (<*> x) f)

instance (Functor f) => Monad (Free f) where
  return :: a -> Free f a
  return = pure

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Free x >>= f = Free (fmap (>>= f) x)

liftF :: (Functor f) => f a -> Free f a
liftF = Free . fmap return

foldFree :: (Monad m) => (forall r. f r -> m r) -> Free f a -> m a
foldFree _ (Pure x) = return x
-- or = interpret x >>= foldFree interpret
-- forall is needed to match the type returned by interpret
-- with the return type
foldFree interpret (Free x) = do
  -- x :: f (Free f a)
  -- x' :: Free f a
  x' <- interpret x
  foldFree interpret x'
