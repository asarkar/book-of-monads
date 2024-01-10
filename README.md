My solutions to the exercises from the book [Book of Monads](https://leanpub.com/book-of-monads), 2nd edition.

[![](https://github.com/asarkar/book-of-monads/workflows/CI/badge.svg)](https://github.com/asarkar/book-of-monads/actions)

A curated list of [Haskell resources](https://blogs.asarkar.com/haskell-curated/) is available on my blog.

## Table of Contents

- [Chapter 0: Introduction](src/Ch00.hs)
- I. What is a Monad?
  - [Chapter 1: Discovering Monads](rc/Ch01.hs)
  - Chapter 2: Better Notation
  - [Chapter 3: Lifting Pure Functions](src/Ch03.hs)
  - [Chapter 4: Utilities for Monadic Code](src/Ch04.hs)
  - [Chapter 5: Interlude: Monad Laws](src/Ch05.md)
- II. More Monads
  - [Chapter 6: Pure Reader-Writer-State Moands](src/Ch06.hs)
  - [Chapter 7: Failure and Logic](src/Ch07.hs)
  - [Chapter 8: Monads for Mutability](src/Ch08.hs)
  - [Chapter 9: Resource Management and Continuations](src/Ch09.md)
- III. Combining Monads
  - [Chapter 10: Functor Composition](src/Ch10.md)
  - [Chapter 11: A Solution: Monad Transformers](src/Ch11)
  - [Chapter 12: Generic Lifting and Unlifiting](src/Ch12.hs)
- IV. Rolling Your Own Monads
  - [Chapter 13: Defining Custom Monads](src/Ch13)
  - Chapter 14: Composing Custom Monads
  - Chapter 15: Performance of Free Monads
- V. Diving into Theory
  - Chapter 16: A Roadmap
  - Chapter 17: Just a Monoid!

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -m <some_word>
```

To run exactly matching tests:
```
./.github/run.sh -m "/<some_word>/"
```

To run a _specific test_:
```
./.github/run.sh -m "/Ch11/evaluates expression/eval/"
```

To run a file containing a `main` method:
```
stack runhaskell app/Main.hs
```

To run an executable listed in `package.yaml`:
```
stack build
stack exec <name>
```

## License

Released under [Apache License v2.0](LICENSE).
