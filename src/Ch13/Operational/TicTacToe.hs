{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Operational.TicTacToe where

import Ch13.TicTacToe
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import qualified Data.Map as Map

data TicTacToe a where
  Info :: Position -> TicTacToe (Maybe Player)
  Take :: Position -> TicTacToe Result
  Done :: a -> TicTacToe a
  Bind :: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b

{-
Exercise 13.12. Write the Functor instance for TicTacToe.
Hint: remember that you can always define fmap f x as x >>= return . f
-}
instance Functor TicTacToe where
  fmap :: (a -> b) -> TicTacToe a -> TicTacToe b
  fmap = (=<<) . (pure .)

instance Applicative TicTacToe where
  pure :: a -> TicTacToe a
  pure = Done

  (<*>) :: TicTacToe (a -> b) -> TicTacToe a -> TicTacToe b
  (<*>) = (. flip fmap) . (>>=)

instance Monad TicTacToe where
  (>>=) :: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b
  (>>=) = Bind

info :: Position -> TicTacToe (Maybe Player)
info = Info

take :: Position -> TicTacToe Result
take = Take

runGame :: TicTacToe a -> ReaderT Player (StateT Board IO) a
runGame (Done x) = return x
runGame (Bind x f) = runGame x >>= runGame . f
runGame (Info p) = S.gets (Map.lookup p)
runGame (Take p) = do
  pl <- S.gets (Map.lookup p)
  case pl of
    Just p' -> return $ AlreadyTaken p'
    Nothing -> do
      pl' <- R.ask
      S.modify (Map.insert p pl')
      return NextTurn
