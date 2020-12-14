.PHONY: clean clean-test clean-pyc clean-build docs help
.DEFAULT_GOAL := help

define BROWSER_PYSCRIPT
import os, webbrowser, sys

try:
	from urllib import pathname2url
except:
	from urllib.request import pathname2url

webbrowser.open("file://" + pathname2url(os.path.abspath(sys.argv[1])))
endef
export BROWSER_PYSCRIPT

define PRINT_HELP_PYSCRIPT
import re, sys

for line in sys.stdin:
	match = re.match(r'^([a-zA-Z_-]+):.*?## (.*)$$', line)
	if match:
		target, help = match.groups()
		print("%-20s %s" % (target, help))
endef
export PRINT_HELP_PYSCRIPT

BROWSER := python -c "$$BROWSER_PYSCRIPT"

CONDA_EXE = ~/miniconda3/condabin/conda
CONDA_ENV_DIR = $(shell cat ${HOME}/.conda/environments.txt | grep envs/pearl)
CONDA_ENV_BIN = $(CONDA_ENV_DIR)/bin/wrappers/conda

COVERAGE = $(PYTHON) -m coverage
FLAKE8 = $(PYTHON) -m flake8
PIP = $(PYTHON) -m pip
PYTEST = $(PYTHON) -m pytest
PYTHON = $(CONDA_ENV_BIN)/python
TOX = $(PYTHON) -m tox
TWINE = $(PYTHON) -m twine

help:
	@python -c "$$PRINT_HELP_PYSCRIPT" < $(MAKEFILE_LIST)

clean: clean-build clean-pyc clean-test ## remove all build, test, coverage and Python artifacts

clean-build: ## remove build artifacts
	rm -fr build/
	rm -fr dist/
	rm -fr .eggs/
	find . -name '*.egg-info' -exec rm -fr {} +
	find . -name '*.egg' -exec rm -f {} +

clean-pyc: ## remove Python file artifacts
	find . -name '*.pyc' -exec rm -f {} +
	find . -name '*.pyo' -exec rm -f {} +
	find . -name '*~' -exec rm -f {} +
	find . -name '__pycache__' -exec rm -fr {} +

clean-test: ## remove test and coverage artifacts
	rm -fr .tox/
	rm -f .coverage
	rm -fr htmlcov/
	rm -fr .pytest_cache

lint: ## check style with flake8
	$(FLAKE8) src tests

test: ## run tests quickly with the default Python
	$(PYTEST)

test-all: ## run tests on every Python version with tox
	$(TOX)

coverage: ## check code coverage quickly with the default Python
	$(COVERAGE) run --source pearllib -m pytest
	$(COVERAGE) report -m
	$(COVERAGE) html
	$(BROWSER) htmlcov/index.html

docs: ## generate Sphinx HTML documentation, including API docs
	rm -f docs/pearl.rst
	rm -f docs/modules.rst
	sphinx-apidoc -o docs/ pearl
	$(MAKE) -C docs clean
	$(MAKE) -C docs html
	$(BROWSER) docs/_build/html/index.html

servedocs: docs ## compile the docs watching for changes
	watchmedo shell-command -p '*.rst' -c '$(MAKE) -C docs html' -R -D .

TWINE_REPOSITORY ?= https://upload.pypi.org/legacy/
release: dist ## package and upload a release
	$(TWINE) upload --repository-url $(TWINE_REPOSITORY) dist/*

release-ci: dist ## package and upload a release from CI
    # @ will not show the command to avoid exposing the password
	@$(TWINE) upload -p $(TWINE_PASSWORD) -u $(TWINE_USER) --repository-url $(TWINE_REPOSITORY) dist/*

dist: clean ## builds source and wheel package
	$(PYTHON) setup.py sdist
	$(PYTHON) setup.py bdist_wheel
	ls -l dist

clean-conda: ## removes conda
	rm -rf ~/miniconda3

install-conda: clean-conda ## install conda
	./bin/install-conda.sh
	$(CONDA_EXE) update --yes conda

PYTHON_VERSION ?= 3.6
init-env-conda: ## init the conda environment
	$(CONDA_EXE) create --yes -n pearl python=$(PYTHON_VERSION)
	$(CONDA_EXE) install --yes -n pearl -c conda-forge conda-wrappers

create-conda-wrappers: ## create wrapper executables to be accessible outside the environment activation
	$(CONDA_ENV_DIR)/bin/create-wrappers -t conda -b $(CONDA_ENV_DIR)/bin -d $(CONDA_ENV_DIR)/bin/wrappers/ --conda-env-dir $(CONDA_ENV_DIR)

upgrade: ## upgrades all dependencies
	$(PIP) install --upgrade -r requirements-dev.in
	$(PIP) freeze | grep -v pearl > requirements-dev.txt

install: clean ## install the package to the active Python's site-packages
	$(PIP) install -e .[dev]
