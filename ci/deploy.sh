#!/usr/bin/env bash

set -e
export TWINE_REPOSITORY="https://test.pypi.org/legacy/"
export TWINE_USER="__token__"

[[ "$TRAVIS_OS_NAME" == "osx" ]] && [[ "$PYTHON_VERSION" == "3.5" ]] && [[ "$TRAVIS_BRANCH" == "master" ]] && make release-ci

export TWINE_REPOSITORY="https://upload.pypi.org/legacy/"

[[ "$TRAVIS_OS_NAME" == "osx" ]] && [[ "$PYTHON_VERSION" == "3.5" ]] && [[ "$TRAVIS_BRANCH" == "master" ]] && make release-ci
