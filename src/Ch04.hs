module Ch04 (zipWithM, replicateM, filterM) where

{-
Exercise 4.1. Write implementations for the last two functions introduced
above in terms of their pure counterparts - zipWith and replicate - composed
with sequence. What goes wrong when you try to do the same with filterM?
-}
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n mx = sequence $ replicate n mx

{-
Can't extract a function (a -> Bool) from (a -> m Bool).
Must be a recursive definition.
-}
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM f (x : xs) = do
  b <- f x
  if b
    then (:) x <$> filterM f xs
    else filterM f xs
