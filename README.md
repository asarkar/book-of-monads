# book-of-monads

[![](https://github.com/asarkar/book-of-monads/workflows/CI/badge.svg)](https://github.com/asarkar/book-of-monads/actions)

Here lay the projects while I was working on the [Book of Monads](https://leanpub.com/book-of-monads) by Alejandro Serrano Mena. I receive no financial incentive for the book purchase.

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

## License

Released under [Apache License v2.0](LICENSE).
