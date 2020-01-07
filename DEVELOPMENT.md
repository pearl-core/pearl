
# Init environment

The following make commands will install conda and create a conda env called `pearl`:

```bash
make install-conda
make init-env-conda
```

# Install Pearl package and its dev dependencies

```bash
make install
```

# Update dependencies
The following step will update the dependencies in `requirements-dev.in` and
include them into the file `requirements-dev.txt`:

```bash
make upgrade
```


# Build the wheel and source distribution

Creates the tar file and wheel under `dist` directory.

```bash
make dist
```

## Publishing

Uses `twine` which separates the build from the actual upload:

```bash
make release
```

