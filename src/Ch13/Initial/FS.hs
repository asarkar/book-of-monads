module Ch13.Initial.FS where

import Ch13.MockFileSystem
import qualified Control.Exception as E
import Control.Monad.State (State)
import qualified Control.Monad.State as S
import qualified Data.Map as Map
import qualified System.IO as IO

data FS a
  = WriteFile FilePath String (Either IOError () -> FS a)
  | ReadFile FilePath (Either IOError String -> FS a)
  | FSDone a

-- After performing some work specific to an operation,
-- we always call the continuation k with the value we computed,
-- which returns a new value.
-- We then interpret this new computation recursively.
interpret :: FS a -> IO a
interpret (FSDone x) = return x
interpret (WriteFile path contents k) =
  (IO.writeFile path contents >> interpret (k $ Right ()))
    `E.catch` \ex -> interpret (k $ Left ex)
interpret (ReadFile path k) =
  do
    contents <- IO.readFile path
    interpret (k (Right contents))
    `E.catch` \ex -> interpret (k (Left ex))

{-
Exercise 13.8. Write an interpretation of FS, mapping it to the State monad.
Hint: base your code on the final style interpretation, as we have done above
for the TicTacToe monad.
-}

interpret' :: FS a -> State MockFileSystem a
interpret' (FSDone x) = return x
interpret' (WriteFile path contents k) = do
  S.modify (MockFileSystem . Map.insert path contents . getMockFileSystem)
  interpret' (k $ Right ())
interpret' (ReadFile path k) = do
  mfs <- S.get
  case Map.lookup path (getMockFileSystem mfs) of
    Just contents -> interpret' (k $ Right contents)
    Nothing ->
      interpret'
        (k . Left . userError $ "Path '" ++ path ++ "' does not exist!")
