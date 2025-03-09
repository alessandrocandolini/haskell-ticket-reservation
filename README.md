[![Compile and run tests](https://github.com/alessandrocandolini/haskell-ticket-reservation/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/haskell-ticket-reservation/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/haskell-ticket-reservation/graph/badge.svg?token=1tCg27i1X1)](https://codecov.io/gh/alessandrocandolini/haskell-ticket-reservation)

# haskell-ticket-reservation

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is by using [ghcup](https://www.haskell.org/ghcup/).
It's also possible to use [the nix package manager](https://nixos.org/), but this project does not provide a nix configuration file.

Assuming `stack` is installed in the system, to **build** the project use
```
stack build
```
To **build and run the tests**, run
```
stack test
```
or equivalently
```
stack build --test
```
For **faster feedback loop** during development, it's possible to run tests continuously on every file change:
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation,
```
stack test --coverage
```
which generates a textual and HTML report.

Note: Tests are run in the CI and test coverage reports are automatically uploaded to codecov.

To **run** the executable via slack,
```
stack exec haskell-ticket-reservation
```
or passing arguments
```
stack exec tickets
```

To **install** the executable under `~/.local/bin`,
```
stack install
```
and the executable can be run with `tickets` assuming `~/.local/bin` is in the `$PATH` variable.

To run a version of **ghci** compatible with the resolver
```
stack ghci
```
For more information about how to use `stack`, refer to the [official docs](https://docs.haskellstack.org/en/stable/).
