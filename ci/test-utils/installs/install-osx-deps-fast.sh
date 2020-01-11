#!/bin/sh

set -ex

brew update
brew install bash zsh grep gnu-sed
# Coreutils and git should be already installed on OSX 7.3+ images:
#brew install coreutils git

BASE_NAME="$(dirname $0)"

"${BASE_NAME}"/install-fish.sh "$TRAVIS_FISH_VERSION"
