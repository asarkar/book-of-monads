{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Ch13.Operational.Stack where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import qualified Data.List as L

{-
Exercise 13.18. Rewrite the IStack monad using the operational/freer construction,
and implement optimize. Is is possible to inspect the computation further in this style?
-}
data IStack r where
  Pop :: IStack Integer
  Push :: Integer -> IStack ()
  Pure :: r -> IStack r
  Bind :: IStack r -> (r -> IStack s) -> IStack s

instance Functor IStack where
  fmap :: (a -> b) -> IStack a -> IStack b
  fmap = (=<<) . (pure .)

instance Applicative IStack where
  pure :: a -> IStack a
  pure = Pure
  (<*>) :: IStack (a -> b) -> IStack a -> IStack b
  (<*>) = (. flip fmap) . (>>=)

instance Monad IStack where
  (>>=) :: IStack a -> (a -> IStack b) -> IStack b
  (>>=) = Bind

data RPNInstruction = Number Integer | Plus | Times | Subtract
  deriving stock (Show)

interpret :: IStack r -> StateT [Integer] Maybe r
interpret (Pure x) = return x
interpret (Bind x f) = interpret x >>= interpret . f
interpret Pop = do
  stack <- S.get
  case L.uncons stack of
    Just (x, xs) -> S.put xs >> pure x
    _ -> S.lift Nothing
interpret (Push x) = S.modify (x :)

evaluate :: [RPNInstruction] -> IStack Integer
evaluate [] = Pop
evaluate ((Number n) : r) = Push n >> evaluate r
evaluate (Plus : r) = ((+) <$> Pop <*> Pop) >>= Push >> evaluate r
evaluate (Times : r) = ((*) <$> Pop <*> Pop) >>= Push >> evaluate r
evaluate (Subtract : r) = (flip (-) <$> Pop <*> Pop) >>= Push >> evaluate r

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions = case S.runStateT (interpret $ evaluate instructions) [] of
  Just (x, _) -> Right x
  Nothing -> Left $ "RPN Instructions don't terminate, " ++ show instructions
