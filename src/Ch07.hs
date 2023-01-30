{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ch07 (pyts) where

import qualified Control.Monad as M
import Control.Monad.Logic (Logic, (>>-))
import qualified Data.Foldable as F
import Prelude hiding (Either (..))

{-
Exercise 7.1. Write the Functor and Monad instances for Either e.
By convention, a successful return value is encoded by Right.
-}

data Either e r = Left e | Right r

instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

instance Applicative (Either e) where
  pure :: a -> Either e a
  pure = Right

  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Monad (Either e) where
  return :: a -> Either e a
  return = pure

  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Right x >>= f = f x
  Left x >>= _ = Left x

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

{-
Exercise 7.2. Write the Alternative instance for Either e.
Hint: you need a default element to build a Left value.
Requiring the error type to be a monoid is a common solution
to this problem.
-}

instance Monoid e => Alternative (Either e) where
  empty = Left mempty
  Right x <|> _ = Right x
  Left _ <|> Right x = Right x
  Left x <|> Left x' = Left (x <> x')

type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels =
  [ ("Alejandro", "Quique"),
    ("Elena", "Quique"),
    ("John", "Mary"),
    ("John", "Tom"),
    ("Mary", "Tim")
  ]

gpgcRels :: [(Person, Person)]
gpgcRels = do
  (grandp, parent) <- pcRels
  (parent', grandc) <- pcRels
  M.guard (parent == parent')
  return (grandp, grandc)

{-
Exercise 7.8. Define siblingRels to return one tuple for
every two people who are siblings, that is, who share a
common parent.
-}
siblingRels :: [(Person, Person)]
siblingRels = do
  (parent, child) <- pcRels
  (parent', sibling) <- pcRels
  M.guard (parent == parent' && child /= sibling)
  return (child, sibling)

list :: [a] -> Logic a
list xs = F.asum (map return xs)

{-
Exercise 7.4. Rewrite the function pyts, which returns Pythagorean triples,
using the Logic monad. Hint: you can reuse the fairTriples auxiliary function.
-}
fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns =
  list ns >>- \x -> list ns >>- \y -> list ns >>- \z -> return (x, y, z)

pyts :: [Integer] -> Logic (Integer, Integer, Integer)
pyts ns =
  fairTriples ns
    >>= \(x, y, z) -> M.guard (x * x + y * y == z * z) >> return (x, y, z)

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- Exercise 7.5 - Implement MonadError for Maybe and Either.
instance MonadError () Maybe where
  throwError _ = Nothing

  Nothing `catchError` f = f ()
  ma `catchError` _ = ma

instance MonadError e (Either e) where
  throwError = Left
  Left l `catchError` h = h l
  Right r `catchError` _ = Right r
