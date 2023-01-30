> Exercise 5.1. The type `Maybe a` has two different monoidal structures.
Can you describe them? Hint: we say that one of these structures is 
right-biased and the other left-biased.

One way is to treat `Maybe a` as a monoid only if its type parameter `a` 
is a monoid as well, and then use `mappend` to join the contents.

```
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

But what if the type of the contents of the `Maybe` aren't an instance of
`Monoid`? Then we can't use `mappend` between them, so we just pick the first
or the second one. Both of the choices are implemented as `First` and `Last`
in `Data.Monoid`.

> Exercise 5.2. Show that the exponentiation function `exp`, which computes
`e^x`, is a monoid homomorphism between the monoid of numbers with addition 
and the monoid of numbers with multiplication.

Given a function `f`, we say it is a monoid homomorphism if it satisfies the
following two laws:

```
         f mempty ≡ mempty
f (x `mappend` y) ≡ (f x) `mappend` (f y)
```

Therefore:

```
exp mempty ≡ exp 0 ≡ 1 ≡ mempty

exp (x `mappend` y) ≡ exp (x + y)
                    ≡ (exp x) * (exp y)
                    ≡ exp x `mappend` exp y
```