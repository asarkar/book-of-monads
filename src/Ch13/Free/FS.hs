{-# LANGUAGE InstanceSigs #-}

module Ch13.Free.FS where

import Ch13.Free.Free (Free)
import qualified Ch13.Free.Free as F
import qualified Control.Exception as E
import Data.Functor ((<&>))
import qualified System.IO as IO

{-
Exercise 13.11. Rewrite FS as a free monad.
Start by introducing the corresponding pattern functor,
and then define the interpreters using foldFree.
-}

data FSF r
  = WriteFile FilePath String (Either IOError () -> r)
  | ReadFile FilePath (Either IOError String -> r)

instance Functor FSF where
  fmap :: (a -> b) -> FSF a -> FSF b
  fmap g (WriteFile path contents f) = WriteFile path contents (g . f)
  fmap g (ReadFile path f) = ReadFile path (g . f)

type FS = Free FSF

interpretFSF' :: FSF a -> IO a
interpretFSF' (WriteFile path contents k) =
  (IO.writeFile path contents >> (pure . k $ Right ()))
    `E.catch` \ex -> pure . k $ Left ex
interpretFSF' (ReadFile path k) =
  (IO.readFile path <&> (k . Right)) `E.catch` (pure . k . Left)

interpretFSF :: FS a -> IO a
interpretFSF = F.foldFree interpretFSF'
