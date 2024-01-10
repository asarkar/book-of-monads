{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Operational.FS where

import Ch13.MockFileSystem
import Control.Monad.State (State)
import qualified Control.Monad.State as S
import qualified Data.Map as Map

-- Exercise 13.13. Rewrite the FS monad in operational style.
data FS a where
  WriteFile :: FilePath -> String -> FS (Either IOError ())
  ReadFile :: FilePath -> FS (Either IOError String)
  Done :: a -> FS a
  Bind :: FS a -> (a -> FS b) -> FS b

instance Functor FS where
  fmap :: (a -> b) -> FS a -> FS b
  fmap = (=<<) . (pure .)

instance Applicative FS where
  pure :: a -> FS a
  pure = Done

  (<*>) :: FS (a -> b) -> FS a -> FS b
  (<*>) = (. flip fmap) . (>>=)

instance Monad FS where
  (>>=) :: FS a -> (a -> FS b) -> FS b
  (>>=) = Bind

runFS :: FS a -> State MockFileSystem a
runFS (Done x) = pure x
runFS (Bind x f) = runFS x >>= runFS . f
runFS (WriteFile path contents) = Right <$> S.modify (MockFileSystem . Map.insert path contents . getMockFileSystem)
runFS (ReadFile path) = do
  mockFs <- S.get
  pure $ case Map.lookup path (getMockFileSystem mockFs) of
    Just result -> Right result
    Nothing -> Left . userError $ "Path '" ++ path ++ "' does not exist!"
