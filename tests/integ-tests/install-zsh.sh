#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget http://downloads.sourceforge.net/project/zsh/zsh/$VERSION/zsh-$VERSION.tar.gz

tar -zxf zsh-$VERSION.tar.gz
cd /tmp/zsh-$VERSION*
./configure
make
sudo make install
