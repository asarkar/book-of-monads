{-# LANGUAGE DerivingStrategies #-}

module Ch13.TicTacToe where

import Data.Map (Map)

{-
Exercise 13.1: Complete the definition of the Position
data type. A value of this type represents a position
in a 3x3 board used to play tic-tac-toe.
-}
data Position = Position Int Int deriving stock (Eq, Ord)

data Player = O | X deriving stock (Eq, Ord, Show)

data Result
  = AlreadyTaken Player
  | NextTurn
  | GameEnded Player

type Board = Map Position Player
