{-# LANGUAGE GADTs #-}

module Ch13.Freer.TicTacToe where

import Ch13.TicTacToe

data TicTacToeI a where
  Info :: Position -> TicTacToeI (Maybe Player)
  Take :: Position -> TicTacToeI Result
