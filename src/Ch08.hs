module Ch08 (addName) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as S
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as T

{-
Exercise 8.1. Modify the code to guarantee that no duplicates exist
in the list of names. In other words, the code should only insert a name
if there is no pair in names that already has it as its second component.
-}
addName' :: TVar Int -> TVar [(Int, String)] -> String -> STM ()
addName' counter names name = do
  i <- T.readTVar counter
  ns <- T.readTVar names
  if any ((== name) . snd) ns
    then return ()
    else T.writeTVar names ((i, name) : ns) >> T.writeTVar counter (i + 1)

pairs :: [String] -> STM (TVar [(Int, String)])
pairs xs = T.newTVar . reverse $ zip [0 ..] xs

next :: Int -> STM (TVar Int)
next = T.newTVar

addName :: [String] -> String -> IO [(Int, String)]
addName names name = do
  let n = length names
  xs <- S.atomically $ pairs names
  m <- S.atomically $ next n
  S.atomically (addName' m xs name)
  T.readTVarIO xs
