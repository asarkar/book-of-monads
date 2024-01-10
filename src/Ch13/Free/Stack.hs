{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Ch13.Free.Stack where

import Ch13.Free.Free (Free)
import qualified Ch13.Free.Free as F
import Control.Monad.State (State)
import qualified Control.Monad.State as S

data IStackF r = Pop (Integer -> r) | Push Integer r
  deriving stock (Functor)

type IStack = Free IStackF

-- Smart constructors.
pop :: IStack Integer
pop = F.liftF (Pop id)

push :: Integer -> IStack ()
push v = F.liftF (Push v ())

data RPNInstruction = Number Integer | Plus | Times | Subtract

evaluate :: [RPNInstruction] -> IStack Integer
evaluate [] = pop
evaluate ((Number n) : r) = push n >> evaluate r
evaluate (Plus : r) = ((+) <$> pop <*> pop) >>= push >> evaluate r
evaluate (Times : r) = ((*) <$> pop <*> pop) >>= push >> evaluate r
evaluate (Subtract : r) = ((-) <$> pop <*> pop) >>= push >> evaluate r

interpret' :: IStackF r -> State [Integer] r
interpret' (Push x r) = do
  S.modify (++ [x])
  pure r
interpret' (Pop ir) = do
  xs <- S.get
  let (s, x) = pop' xs
  S.put s
  pure $ ir x
  where
    pop' :: [a] -> ([a], a)
    pop' [] = error "Two values must be on the stack to call operations"
    pop' xs = (init xs, last xs)

{-
Exercise 13.17. Write an interpreter from IStack to State [Integer].
Use that interpreter to write a function from [RPNInstruction] to Integer.
-}
interpret :: IStack r -> State [Integer] r
interpret = F.foldFree interpret'

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions =
  let (x, s) = S.runState (interpret $ evaluate instructions) []
   in if null s
        then Right x
        else Left $ "RPN Instructions don't terminate, stack: " ++ show s ++ " and value: " ++ show x
