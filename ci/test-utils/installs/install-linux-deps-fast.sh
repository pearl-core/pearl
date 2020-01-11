#!/bin/sh

set -ex

sudo apt-get -qq update
sudo apt-get install -y zsh bash git

BASE_NAME="$(dirname $0)"

"${BASE_NAME}"/install-fish.sh "$TRAVIS_FISH_VERSION"
