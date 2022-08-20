PYPI_USER ?= "__token__"
PYPI_REPOSITORY ?= "https://upload.pypi.org/legacy/"


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
	rm -f .coverage
	rm -fr htmlcov/
	rm -fr .pytest_cache

format:
	poetry run black src tests
	poetry run isort -v src tests

lint: ## check style with flake8
	poetry run mypy
	# Check that source is properly formatted
	poetry run black --check src tests
	# Check for linting issues
	poetry run flake8 src/**/*.py tests/**/*.py
	# Make sure imports are properly sorted
	poetry run isort --check -v src tests

bandit:
	poetry run bandit -lll -r tests src

test: ## run tests quickly with the default Python
	poetry run pytest src tests

test-integration:
	bash ./integ-tests/integ-tests.sh $(PWD)

shellcheck:
	shellcheck src/pearllib/static/boot/sh/pearl.sh src/pearllib/static/builtins/utils.sh integ-tests/integ-tests.sh

build:
	poetry build
	ls -l dist

install:
	poetry install

update:
	poetry update

publish: build ## package and upload a release
    # @ will not show the command to avoid exposing the password
	@poetry publish --username $(PYPI_USER) --password $(PYPI_PASSWORD) --repository $(PYPI_REPOSITORY) --no-interaction $(ARGS)

coverage: ## check code coverage quickly with the default Python
	$(COVERAGE) run --source pearllib -m pytest
	$(COVERAGE) report -m
	$(COVERAGE) html
	$(BROWSER) htmlcov/index.html

.DEFAULT_GOAL := default

default: install format bandit test shellcheck build
default-ci: install format lint bandit test shellcheck test-integration build

.PHONY: clean clean-test clean-pyc clean-build docs install format lint bandit test test-integration build
