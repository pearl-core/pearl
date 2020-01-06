#!/bin/sh
set -ex

# Gettext is required for installing git from source code
sudo apt-get install gettext

./integ-tests/install-bash.sh "$TRAVIS_BASH_VERSION"
./integ-tests/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./integ-tests/install-fish.sh "$TRAVIS_FISH_VERSION"
./integ-tests/install-git.sh "$TRAVIS_GIT_VERSION"
