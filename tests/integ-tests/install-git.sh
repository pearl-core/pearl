#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget https://github.com/git/git/archive/v$VERSION.tar.gz

tar -zxf v$VERSION.tar.gz
cd /tmp/git-$VERSION*
# To fix the problem of missing libintl in OSX (http://stackoverflow.com/a/11370808/967489):
export NO_GETTEXT=1
make configure
./configure --prefix=/usr
make all
sudo make install
