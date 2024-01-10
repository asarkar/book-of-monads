{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Final.FS where

import Ch13.MockFileSystem
import qualified Control.Exception as E
import Control.Monad.State (State)
import qualified Control.Monad.State as S
import qualified Data.Map as Map

class FS m where
  writeFile :: FilePath -> String -> m (Either IOError ())
  readFile :: FilePath -> m (Either IOError String)

-- Cannot mix FS and IO actions freely
-- need to specialize m ~ IO for this
-- to work
--
-- f :: (Monad m, FS m) => FilePath -> m ()
-- f path = do
--   num <- randomRIO (1 :: Integer, 100)
--   void $ Lib.writeFile path (show num)

instance FS IO where
  writeFile :: FilePath -> String -> IO (Either IOError ())
  writeFile path contents =
    (Right <$> Prelude.writeFile path contents) `E.catch` \ex -> return $ Left ex

  readFile :: FilePath -> IO (Either IOError String)
  readFile path =
    (Right <$> Prelude.readFile path) `E.catch` \ex -> return $ Left ex

-- Exercise 13.4. Complete the interpretation of FS as a State monad.
instance FS (State MockFileSystem) where
  writeFile :: FilePath -> String -> State MockFileSystem (Either IOError ())
  writeFile path contents =
    Right
      <$> S.modify (MockFileSystem . Map.insert path contents . getMockFileSystem)

  readFile :: FilePath -> State MockFileSystem (Either IOError String)
  readFile path = do
    mfs <- S.gets getMockFileSystem
    pure $ case Map.lookup path mfs of
      Nothing -> Left . userError $ "Path '" ++ path ++ "' does not exist!"
      Just contents -> Right contents
