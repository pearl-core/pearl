#!/bin/sh
set -ex

# Gettext is required for installing git from source code
sudo apt-get install gettext

BASE_NAME="$(dirname $0)"

"${BASE_NAME}"/install-bash.sh "$TRAVIS_BASH_VERSION"
"${BASE_NAME}"/install-zsh.sh "$TRAVIS_ZSH_VERSION"
"${BASE_NAME}"/install-fish.sh "$TRAVIS_FISH_VERSION"
"${BASE_NAME}"/install-git.sh "$TRAVIS_GIT_VERSION"
