module Ch03 (ap, ZipList (..), mapZ) where

import Prelude hiding (Monad, return, (>>=))

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

{- HLINT ignore "Replace case with maybe" -}
{-
Found:
  case v of
    Nothing -> Nothing
    Just v' -> g v'
Perhaps:
  maybe Nothing g v
-}
then_ :: Maybe a -> (a -> Maybe b) -> Maybe b
then_ v g = case v of
  Nothing -> Nothing
  Just v' -> g v'

instance Monad Maybe where
  return = Just
  (>>=) = then_

-- Exercise3.1. Write the implementation of ap.
{- HLINT ignore "Use <&>" -}
{-
Found:
  x >>= (return . f')
Perhaps:
  x Data.Functor.<&> f'
-}
ap :: Monad m => m (b -> c) -> m b -> m c
ap f x = f >>= (\f' -> x >>= (return . f'))

{- -Wunused-top-binds
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a ->b) -> f a -> f b
-}

{-
Exercise 3.2. You do not need fmap at all in Applicative.
Write fmap as a combination of pure and ap.

fmap f = (ap . pure f)
-}

newtype ZipList a = ZipList [a] deriving (Show, Eq)

-- Exercise 3.8. Write the Functor instance for ZipList.
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

mapZ :: (a -> b) -> ZipList a -> ZipList b
mapZ = fmap

{-
Exercise 3.4. Write the functions that perform the conversions between the
"normal" implementations oftriples and 4-tuplesand their implementations as
nested pairs.

tuple3 :: (a, b, c) -> ((a, b), c)
tuple3 (x, y, z) = ((x, y), z)

tuple4 :: (a, b, c, d) -> ((a, b), (c, d))
tuple4 (i, j, k, l) = ((i, j), (k, l))
-}

{-
Implementation of Applicative in terms of Monoidal:

unit returns f (), we discard the () and return x,

pure :: a -> f a
pure x = fmap (\_ -> x) unit

fx is a Functor containing a function, fx ** fy creates
a Functor where the the first element of the tuple
is the function in fx. We simply apply it on the second
element within fmap.

(<*>) :: f (a -> b) -> f a -> f b
fx <*> fy = fmap (\(g, k) -> g k) (fx ** fy)
-}

{-
Exercise 3.5. Write the other direction of the conversion. In other words,
give definitions of unit and (**) in terms of fmap, pure, and (<*>).

unit :: f ()
unit = pure ()

(**) :: f a -> f b -> f (a, b)
(**) fx fy = (,) <$> fx <*> fy
-}
