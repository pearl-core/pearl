#!/bin/sh
set -ex
brew update
brew install grep gnu-sed
# Coreutils and git should be already installed on OSX 7.3+ images:
#brew install coreutils git
# Openssl is required for installing git from source code
# Findutils is required for `fonts` Pearl package
brew install openssl findutils

BASE_NAME="$(dirname $0)"

"${BASE_NAME}"/install-bash.sh "$TRAVIS_BASH_VERSION"
"${BASE_NAME}"/install-zsh.sh "$TRAVIS_ZSH_VERSION"
"${BASE_NAME}"/install-fish.sh "$TRAVIS_FISH_VERSION"
"${BASE_NAME}"/install-git.sh "$TRAVIS_GIT_VERSION"

