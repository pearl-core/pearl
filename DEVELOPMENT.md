
# Init environment

Follow steps to install miniconda [here](https://docs.conda.io/projects/conda/en/latest/index.html).

The following make commands `init` will update conda, create a conda env called `pearl` and install all stable version
dependencies:

```
make conda-init
make init
```

# Install Pearl package in site-packages

```
make install
```

# Update dependencies
The following step will update the dependencies in `requirements-dev.in` and
include them into the file `requirements-dev.txt`:

```
make upgrade
```


# Build the wheel and source distribution

Creates the tar file and wheel under `dist` directory.

```
make dist
```

## Publishing

Uses `twine` which separates the build from the actual upload:

```bash
make release
```

