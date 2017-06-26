#!/bin/sh
set -ex

# Gettext is required for installing git from source code
sudo apt-get install gettext

./tests/test-utils/install-bash.sh "$TRAVIS_BASH_VERSION"
./tests/test-utils/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./tests/test-utils/install-fish.sh "$TRAVIS_FISH_VERSION"
./tests/test-utils/install-git.sh "$TRAVIS_GIT_VERSION"
