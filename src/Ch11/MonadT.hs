{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Ch11.MonadT (ExT (..), MbT (..), StT (..), runStT) where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State (StateT (..))
import Data.Functor.Classes (Eq1, Read1, Show1)
import qualified Data.Functor.Classes as FC

{-
Exercise 11.2. Write the Monad instances for the combinations
of the above transformers with an arbitrary monad.
-}

-- implement all instances manually
newtype ExT e m a = ExT (m (Either e a))

runExT :: ExT e m a -> m (Either e a)
runExT (ExT x) = x

{-
Note the use of `Eq1`. `Eq` accepts a concrete type,
while `m` takes a concrete type and returns another,
and hence, can't be used with `Eq`.
Same logic applies for `Show1` and `Read1` below.
-}
instance (Eq e, Eq1 m, Eq a) => Eq (ExT e m a) where
  (==) :: ExT e m a -> ExT e m a -> Bool
  (==) x y = runExT x `FC.eq1` runExT y

instance (Show e, Show1 m, Show a) => Show (ExT e m a) where
  showsPrec :: Int -> ExT e m a -> ShowS
  showsPrec d x = FC.showsUnaryWith FC.showsPrec1 "ExT" d (runExT x)

instance (Read e, Read1 m, Read a) => Read (ExT e m a) where
  readsPrec :: Int -> ReadS (ExT e m a)
  readsPrec = FC.readsData $ FC.readsUnaryWith FC.readsPrec1 "ExT" ExT

instance (Functor m) => Functor (ExT e m) where
  fmap :: (a -> b) -> ExT e m a -> ExT e m b
  fmap f = ExT . fmap (fmap f) . runExT

instance (Applicative m) => Applicative (ExT e m) where
  pure :: a -> ExT e m a
  pure x = ExT $ pure $ Right x

  (<*>) :: ExT e m (a -> b) -> ExT e m a -> ExT e m b
  {-
  Combine the output of `runExT` (i.e. `m`) by running `fmap`
  and using `(<*>)` inside `fmap`. Basically, we use `runExT`
  to peel off `ExT`, and `fmap` to peel off `m`.
  -}
  (<*>) f x = ExT $ (<*>) <$> runExT f <*> runExT x

instance (Monad m) => Monad (ExT e m) where
  return :: a -> ExT e m a
  return = pure

  (>>=) :: ExT e m a -> (a -> ExT e m b) -> ExT e m b
  x >>= f = ExT $ do
    y <- runExT x
    case y of
      {-
      since we are in the context of monad x (from runExT above),
      `return` creates a m (Either e b)
      -}
      Left ex -> return (Left ex)
      {-
      f k :: ExT e m b
      runExT (f k) :: m (Either e b)
      -}
      Right k -> runExT (f k)

  -- required by Monad law
  (>>) :: ExT e m a -> ExT e m b -> ExT e m b
  (>>) = (*>)

-- derive instances
newtype MbT m a = MbT (m (Maybe a))
  deriving (Show, Read, Eq) via MaybeT m a
  deriving (Functor, Applicative) via MaybeT m

-- deriving stock instance (Show (m (Maybe a))) => Show (MbT m a)
-- deriving stock instance (Read (m (Maybe a))) => Read (MbT m a)
-- deriving stock instance (Eq (m (Maybe a))) => Eq (MbT m a)

runMbT :: MbT m a -> m (Maybe a)
runMbT (MbT x) = x

instance (Monad m) => Monad (MbT m) where
  return :: a -> MbT m a
  return = pure

  (>>=) :: MbT m a -> (a -> MbT m b) -> MbT m b
  x >>= f = MbT $ do
    y <- runMbT x
    case y of
      Just k -> let j = f k in runMbT j
      _ -> return Nothing

  (>>) :: MbT m a -> MbT m b -> MbT m b
  (>>) = (*>)

newtype StT s m a = StT (s -> m (a, s))
  deriving (Functor, Applicative) via StateT s m

runStT :: StT s m a -> (s -> m (a, s))
runStT (StT x) = x

instance (Monad m) => Monad (StT s m) where
  return :: a -> StT s m a
  return = pure

  (>>=) :: StT s m a -> (a -> StT s m b) -> StT s m b
  x >>= f = StT $ \s -> do
    (k, s') <- runStT x s
    runStT (f k) s'

  (>>) :: StT s m a -> StT s m b -> StT s m b
  (>>) = (*>)
