
## Install Requirements

- Follow the steps to install Poetry [here](https://python-poetry.org/docs/#installation).
- Install [shellcheck](https://github.com/koalaman/shellcheck)

## Init environment

Running `make` without any target will create
the poetry virtual env, run several checks
including the unit tests:

```
make
```

## Install Pearl package and its dev dependencies

```bash
make install
```

## Update dependencies
The following step will update the dependencies in `poetry.lock`:

```bash
make update
```


## Build the wheel and source distribution

Creates the tar file and wheel under `dist` directory.

```bash
make build
```

## Publishing

To publish to test PYPI:

```bash
PYPI_PASSWORD="<token here>" make publish-test
```

To publish to production PYPI:

```bash
PYPI_PASSWORD="<token here>" make publish
```

