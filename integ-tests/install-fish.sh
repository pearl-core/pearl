#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget https://github.com/fish-shell/fish-shell/releases/download/$VERSION/fish-$VERSION.tar.gz
tar -xzf fish-$VERSION.tar.gz
cd /tmp/fish-$VERSION && ./configure && make && sudo make install
