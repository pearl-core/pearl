#!/bin/sh
set -ex

./tests/integ-tests/install-bash.sh "$TRAVIS_BASH_VERSION"
./tests/integ-tests/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./tests/integ-tests/install-fish.sh "$TRAVIS_FISH_VERSION"
./tests/integ-tests/install-git.sh "$TRAVIS_GIT_VERSION"
