#!/bin/sh
set -ex
brew update
# Coreutils and git should be already installed on OSX 7.3+ images:
#brew install coreutils git
# Openssl is required for installing git from source code
# Findutils is required for `fonts` Pearl package
brew install openssl findutils

./tests/test-utils/install-bash.sh "$TRAVIS_BASH_VERSION"
./tests/test-utils/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./tests/test-utils/install-fish.sh "$TRAVIS_FISH_VERSION"
./tests/test-utils/install-git.sh "$TRAVIS_GIT_VERSION"

