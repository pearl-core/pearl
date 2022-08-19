#!/usr/bin/env bash

set -e

# TODO Test the deploy
if [[ "$TRAVIS_OS_NAME" == "osx" && "$PYTHON_VERSION" == "3.7" && "$TRAVIS_BRANCH" == "master" ]]
then
    export TWINE_USER="__token__"

    export TWINE_REPOSITORY="https://test.pypi.org/legacy/"
    TWINE_PASSWORD="${TWINE_TEST_PASSWORD}" make release-ci

    export TWINE_REPOSITORY="https://upload.pypi.org/legacy/"
    make release-ci
fi
