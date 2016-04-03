#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget http://fishshell.com/files/$VERSION/fish-$VERSION.tar.gz
tar -xzvf fish-$VERSION.tar.gz
cd /tmp/fish-$VERSION && ./configure && make && sudo make install
