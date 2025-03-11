[![Compile and run tests](https://github.com/alessandrocandolini/haskell-ticket-reservation/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/haskell-ticket-reservation/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/haskell-ticket-reservation/graph/badge.svg?token=1tCg27i1X1)](https://codecov.io/gh/alessandrocandolini/haskell-ticket-reservation)

# haskell-ticket-reservation

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is via [ghcup](https://www.haskell.org/ghcup/).
Alternatively, it is possible to install stack using [the nix package manager](https://nixos.org/), but this project does not provide a nix configuration file.

Assuming `stack` is installed in the system, to **compile** the project use
```
stack build
```
To **compile and run the tests**, use
```
stack test
```
⚠️ Running the application or tests locally requires a Redis instance available at `localhost:6379`. See the [Docker Setup](README.md#docker-setup) section below for instructions on setting one up via Docker. 

For a **faster development loop**, it is possible to continuously run tests on file changes using
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation, use
```
stack test --coverage
```
This generates both a textual and an HTML report.

📌 Tests are automatically executed in CI, and coverage reports are uploaded to Codecov.

To **run** the application via slack, use
```
stack exec tickets
```
or, if passing arguments,
```
stack exec tickets
```

To **install** the executable under `~/.local/bin`, use 
```
stack install
```
Once installed, the executable can be run using the `tickets` command, assuming `~/.local/bin` is in the `$PATH`.

To start a **GHCi** session compatible with the project's resolver, use 
```
stack ghci
```
For more details on how to use `stack`, refer to the [official docs](https://docs.haskellstack.org/en/stable/).


## Docker setup

A local Redis instance is required to run the application or tests. A `docker-compose.yml` file is provided to simplify the setup.

Assuming `docker` is installed and the Docker daemon is running, start Redis with:
```
docker compose up -d
```
To tear down the container, use 
```
docker compose down
```

The CI pipeline follows the same process.
