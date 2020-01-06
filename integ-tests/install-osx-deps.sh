#!/bin/sh
set -ex
brew update
brew install grep gnu-sed
# Coreutils and git should be already installed on OSX 7.3+ images:
#brew install coreutils git
# Openssl is required for installing git from source code
# Findutils is required for `fonts` Pearl package
brew install openssl findutils

./integ-tests/install-bash.sh "$TRAVIS_BASH_VERSION"
./integ-tests/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./integ-tests/install-fish.sh "$TRAVIS_FISH_VERSION"
./integ-tests/install-git.sh "$TRAVIS_GIT_VERSION"

