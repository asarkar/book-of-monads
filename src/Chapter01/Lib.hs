module Chapter01.Lib (Tree (..), relabel, (++), map) where

import Prelude hiding (map, pure, (++))

type State s a = s -> (a, s)

{-
Exercise 1.1. Rewrite the definitions of pure and next to
work with an arbitrary stateful computation State s a.
-}
next :: State s a -> (a -> State s b) -> State s b
-- next :: s -> (a, s) -> (a -> s -> (b, s)) -> s -> (b, s)
f `next` g = \state -> let (x, nextState) = f state in g x nextState

pure :: a -> State s a
-- pure :: a -> s -> (a, s)
pure = (,)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

relabel :: Tree a -> State Int (Tree (Int, a))
relabel (Leaf x) = \i -> (Leaf (i, x), i + 1)
{-
'next' takes a state and a function to modify the state,
and returns the modified state.
We call 'next' on the left and right subtrees,
and then return a state with the modified subtrees.
This can be better understood if the 'next' calls are
thought of as binds in a 'do' block. State here isn't
a Monad (yet), so we can't use 'do' notation.
-}
relabel (Node l r) =
  relabel l `next` \l' ->
    relabel r `next` \r' ->
      pure (Node l' r')

{-
Exercise 1.2. Write a function (++) that takes two lists and returns
its concatenation. That is, given two lists l1 and l2, l1 + l2 contains
the elements of l1 followed by the elements of l2.
-}
(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

-- Exercise 1.3. Do you remember how map is defined? Try it out!
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

{-
Exercise 1.4. Convince yourself that the two definitions of flatten are
equivalent by expanding the code of then_ in the second one.

flatten x = then_ x id
=> flatten x = then_ x (\y -> y)
If the value contained in x is a Maybe, that's what we get
-}
