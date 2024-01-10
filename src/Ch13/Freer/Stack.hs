{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Ch13.Freer.Stack where

import Control.Monad ((<=<))
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import qualified Data.List as L

data IStackI r where
  Pop :: IStackI Integer
  Push :: Integer -> IStackI ()

type IStack = Freer IStackI

pop :: IStack Integer
pop = Impure Pop return

push :: Integer -> IStack ()
push n = Impure (Push n) Pure

interpret' :: IStackI r -> StateT [Integer] Maybe r
interpret' Pop = do
  stack <- S.get
  case L.uncons stack of
    Just (x, xs) -> S.put xs >> pure x
    _ -> S.lift Nothing
interpret' (Push x) = S.modify (x :)

data RPNInstruction = Number Integer | Plus | Times | Subtract deriving stock (Show)

interpret :: IStack r -> StateT [Integer] Maybe r
interpret = foldFreer interpret'

evaluate :: [RPNInstruction] -> IStack Integer
evaluate [] = pop
evaluate ((Number n) : r) = push n >> evaluate r
evaluate (Plus : r) = (+) <$> pop <*> pop >>= push >> evaluate r
evaluate (Times : r) = (*) <$> pop <*> pop >>= push >> evaluate r
evaluate (Subtract : r) = flip (-) <$> pop <*> pop >>= push >> evaluate r

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions = case S.runStateT (interpret $ evaluate instructions) [] of
  Just (x, _) -> Right x
  Nothing -> Left $ "RPN Instructions don't terminate, " ++ show instructions

data Freer instr a where
  Pure :: a -> Freer instr a
  Impure :: instr a -> (a -> Freer instr b) -> Freer instr b

instance Functor (Freer instr) where
  fmap :: (a -> b) -> Freer instr a -> Freer instr b
  fmap f (Pure x) = Pure $ f x
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (Freer instr) where
  pure :: a -> Freer instr a
  pure = Pure

  (<*>) :: Freer instr (a -> b) -> Freer instr a -> Freer instr b
  Pure f <*> Pure x = Pure $ f x
  Impure x k <*> f = Impure x (\a -> k a <*> f)
  Pure f <*> Impure x k = Impure x (fmap f . k)

instance Monad (Freer instr) where
  (>>=) :: Freer instr a -> (a -> Freer instr b) -> Freer instr b
  Pure x >>= f = f x
  Impure x k >>= f = Impure x (f <=< k)

foldFreer :: (Monad m) => (forall r. f r -> m r) -> Freer f a -> m a
foldFreer _ (Pure x) = return x
foldFreer i (Impure x f) = i x >>= foldFreer i . f
