{-# LANGUAGE InstanceSigs #-}

module Ch13.Free.TicTacToe where

import Ch13.Free.Free (Free)
import qualified Ch13.Free.Free as F
import Ch13.TicTacToe
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import qualified Data.Map as Map

data TicTacToeF r
  = Info Position (Maybe Player -> r)
  | Take Position (Result -> r)

-- Exercise 13.9. Write the Functor instance for TicTacToeF.
-- Can be derived automatically by 'deriving Functor'.
instance Functor TicTacToeF where
  fmap :: (a -> b) -> TicTacToeF a -> TicTacToeF b
  fmap g (Info p f) = Info p (g . f)
  fmap g (Take p f) = Take p (g . f)

type TicTacToe = Free TicTacToeF

info :: Position -> Free TicTacToeF (Maybe Player)
info p = F.liftF (Info p id)

runGame' :: TicTacToeF a -> ReaderT Player (StateT Board IO) a
runGame' (Info p k) = do
  pl <- S.gets (Map.lookup p)
  return (k pl)
-- or = k <$> (lookup p <$> get)
-- or = k . lookup p <$> get
runGame' (Take p k) = do
  pl <- S.gets (Map.lookup p)
  case pl of
    Just pl' -> return $ k $ AlreadyTaken pl'
    Nothing -> do
      pl' <- R.ask
      S.modify (Map.insert p pl')
      return $ k NextTurn

runGame :: TicTacToe a -> ReaderT Player (StateT Board IO) a
runGame = F.foldFree runGame'
