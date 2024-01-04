{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Ch11.Lib
  ( eval,
    eval',
    Expr (..),
    Op (..),
    Assignment,
    Identity (..),
    toIdentity,
    fromIdentity,
    parseHex,
  )
where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State (State, StateT (..))
import qualified Control.Monad.Trans.State as S
import qualified Data.List as L
import qualified Data.Maybe as Mb

type Name = String

data Expr = Literal Integer | Var Name | Op Op Expr Expr

data Op = Add | Subtract | Multiply | Divide

instance Show Op where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "`div`"

instance Show Expr where
  show (Literal i) = show i
  show (Var x) = x
  show (Op o l r) = "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"

type Assignment = [(Name, Integer)]

eval :: Expr -> Assignment -> Maybe Integer
eval (Literal n) _ = return n
eval (Var v) a = L.lookup v a
eval (Op o x y) a = do
  u <- eval x a
  v <- eval y a
  case o of
    Add -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide ->
      if v == 0
        then Nothing
        else return (u `div` v)

{-
Exercise 11.1. Rewrite the last implementation ofthe eval function
to have this type:
eval :: Expr -> MaybeT (State Assignment) Integer

`MaybeT` wraps a `Maybe a` (in this case `Integer`) with a Monad
(in this case, `State`).
`return` from `MaybeT` is defined as `return = MaybeT . return . Just`.
Thus, we only need to return an `Integer` for the return type to work.
-}
eval' :: Expr -> MaybeT (State Assignment) Integer
eval' (Literal n) = return n
{-
`gets :: (s -> a) -> State s a`.
Thus, we don't need the last two functions from the definition of `return`,
and we use the `MaybeT` type constructor directly.
-}
eval' (Var v) = MaybeT $ S.gets (L.lookup v)
eval' (Op o x y) = do
  u <- eval' x
  v <- eval' y
  case o of
    Add -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide -> do
      M.guard (v /= 0)
      return (u `div` v)

{-
Exercise 11.8. Check that T and Identity T are isomorphic for any type T.
In other words, write these two functions:

toIdentity :: a -> Identity a
fromIdentity :: Identity a -> a

Check that applying one after the other, in any order, always gives back
the same result.
-}
newtype Identity a = I a
  deriving stock (Eq, Show)

toIdentity :: a -> Identity a
toIdentity = I

fromIdentity :: Identity a -> a
fromIdentity (I x) = x

{-
Exercise 11.4. Prove (or at least convince yourself)
that the definitions of return and fmap also coincide.

return x
≡ MaybeT (I x)
≡ I (Just x)

-- f :: a -> b
fmap f (MaybeT (I a))
≡ fmap f (I (Maybe a))
≡ I $ \x ->
  case x of
    Just a -> f a
    Nothing -> Nothing
≡ I (Maybe b)
-}

{-
We build a hex parser using a Monad Transformer.

Exercise 11.5. Actually, we can also take char as the primitive operation.
Write char directly in terms of State. Then, use char and the guard
function to define satisfies.

Unlike the book, we choose the Monad as `Maybe`, not `[]`, because each parser
produces at most one result.
-}
type Parser a = StateT String Maybe a

char :: Parser Char
char = StateT $ \case
  [] -> A.empty
  (x : xs) -> pure (x, xs)

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- char
  M.guard $ p c
  pure c

many :: Parser Char -> Parser String
{-
in the final recursive call, when the input string is "",
c <- p fails. then return `empty` as the parse result.
-}
many p = (:) <$> p <*> many p A.<|> return A.empty

hex :: Parser String
hex = zero *> x *> many digit
  where
    zero = satisfies (== '0')
    x = satisfies (`elem` "xX")
    digit = satisfies (`elem` "abcdefABCDEF0123456789")

parseHex :: String -> String
parseHex = Mb.fromMaybe "" . S.evalStateT hex
