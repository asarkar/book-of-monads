My solutions to the exercises from the book [Book of Monads](https://leanpub.com/book-of-monads), 2nd edition.

[![](https://github.com/asarkar/book-of-monads/workflows/CI/badge.svg)](https://github.com/asarkar/book-of-monads/actions)

A curated list of [Haskell resources](https://blogs.asarkar.com/haskell-curated/) is available on my blog.

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
