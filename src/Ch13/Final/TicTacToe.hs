{-# LANGUAGE DerivingStrategies #-}
-- This is needed to declare TicTacToe instance with
-- actual type constructors instead of type variables.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Final.TicTacToe where

import Ch13.TicTacToe
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import qualified Data.Map as Map

{-
Exercise 13.3. Implement an algebra in final style for the
custom monad you devised in the previous section.
Write a couple of computations that work in that monad.
-}

-- Syntax/Algebra
class TicTacToe m where
  info :: Position -> m (Maybe Player)
  take :: Position -> m Result

-- Interpreter
instance TicTacToe (ReaderT Player (StateT Board IO)) where
  info :: Position -> ReaderT Player (StateT Board IO) (Maybe Player)
  info = S.gets . Map.lookup

  take :: Position -> ReaderT Player (StateT Board IO) Result
  take p = do
    i <- info p
    case i of
      Just p' -> return $ AlreadyTaken p'
      _ -> do
        pl <- R.ask
        S.modify (Map.insert p pl)
        S.liftIO $ putStrLn "Your next move:"
        pure NextTurn

-- endOrCont :: MonadState Board m => Player -> m Result
-- endOrCont pl = do
--   b <- S.get
--   if match b rows || match b cols || match b [diag] || match b [antiDiag]
--     then return $ GameEnded pl
--     else return NextTurn
--   where
--     pos = uncurry Position
--     row r = map (pos . (r, )) [1..3]
--     rows = map row [1..3]
--     col c = map (pos . (, c)) [1..3]
--     cols = map col [1..3]
--     diag = map (pos . M.join (,)) [1..3]
--     antiDiag = map pos [(1, 3), (2, 2), (3, 1)]
--     match b = any (all ((Just pl ==) . flip Map.lookup b))

-- Applying interpreter.
-- The book doesn't say how the game will be
-- played using the TicTacToe typeclass.
logic :: m Result
logic = undefined

runTicTacToe :: IO (Result, Board)
runTicTacToe = S.runStateT (R.runReaderT logic X) Map.empty
