```
{-# LANGUAGE RankNTypes #-}

module Chapter09.Lib () where

newtype Cont r a = Cont
  { runCont :: (a -> r) -> r
  }
```

`forall` is required because `r` is chosen by the callee.
See [RankNTypes, Rank2Types, and PolymorphicComponents](https://www.schoolofhaskell.com/user/PthariensFlame/guide-to-ghc-extensions/explicit-forall#rankntypes--rank2types--and-polymorphiccomponents).

`f` here is the function `(a -> r)`.

```
toCont :: a -> (forall r. Cont r a)
toCont a = Cont $ \f -> f a
```

The first argument to `f` is a function `(a -> r)`.
In this case, we supply `id`, which is `(a -> a)`.
Since the output of `f` is the same type as the 
output of the function, the output is also `a`.

```
fromCont :: (forall r. Cont r a) -> a
fromCont (Cont f) = f id
```
