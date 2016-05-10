#!/bin/sh
set -ex

brew update
brew install coreutils

./tests/integ-tests/install-bash.sh "$TRAVIS_BASH_VERSION"
./tests/integ-tests/install-zsh.sh "$TRAVIS_ZSH_VERSION"
./tests/integ-tests/install-fish.sh "$TRAVIS_FISH_VERSION"
sudo rm -f /bin/bash
sudo ln -s /usr/local/bin/bash /bin/bash
sudo rm -f /bin/zsh
sudo ln -s /usr/local/bin/zsh /bin/zsh

echo "PATH=/usr/local/opt/coreutils/libexec/gnubin:\$PATH" >> ${HOME}/.bashrc
echo "PATH=/usr/local/opt/coreutils/libexec/gnubin:\$PATH" >> ${HOME}/.zshrc
mkdir -p ${HOME}/.config/fish
echo "set -x PATH /usr/local/opt/coreutils/libexec/gnubin \$PATH" >> ${HOME}/.config/fish/config.fish

./tests/integ-tests/install-git.sh "$TRAVIS_GIT_VERSION"

