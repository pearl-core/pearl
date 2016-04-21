#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget https://github.com/git/git/archive/v$VERSION.tar.gz

tar -zxvf v$VERSION.tar.gz
cd /tmp/git-$VERSION*
make all
sudo make install
