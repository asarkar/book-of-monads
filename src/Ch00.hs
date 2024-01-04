{-# LANGUAGE InstanceSigs #-}

module Ch00 (eqTuple, eqList, notEq, isEmptyList, insertList) where

import Prelude hiding (Eq (..))

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  (==) :: Bool -> Bool -> Bool
  True == True = True
  False == False = True
  _ == _ = False

instance (Eq a) => Eq [a] where
  (==) :: [a] -> [a] -> Bool
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  _ == _ = False

-- Exercise 0.2. Define the Eq instance for tuples (a, b).
instance (Eq a, Eq b) => Eq (a, b) where
  (==) :: (a, b) -> (a, b) -> Bool
  (x, y) == (x', y') = x == x' && y == y'

eqTuple :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
eqTuple x y = x == y

eqList :: (Eq a) => [a] -> [a] -> Bool
eqList [] [] = True
eqList (x : xs) (y : ys) = x == y && eqList xs ys
eqList _ _ = False

{-
Exercise 0.3. Define the function notEq, which returns False
if the given arguments are not equal.
You should use the Eq trait defined above.
-}
notEq :: (Eq a) => a -> a -> Bool
{- HLINT ignore "Use /=" -}

notEq x y = not (x == y)

class Container c where
  empty :: c a
  insert :: a -> c a -> c a

-- Exercise 0.5. Write the List instance for the Container type class.
instance Container [] where
  empty :: [a]
  empty = []
  insert :: a -> [a] -> [a]
  insert = (:)

isEmptyList :: (Eq a) => [a] -> Bool
isEmptyList xs = xs == empty

insertList :: a -> [a] -> [a]
insertList = insert

{- -Wunused-top-binds
newtype Queue a = Queue
  { unQueue :: [a]
  }

instance Container Queue where
  empty = Queue []

  -- insert x (Queue xs) = Queue $ xs ++ [x]
  insert x xs = Queue $ unQueue xs ++ [x]
-}
