#!/bin/sh
set -ex

VERSION=$1

cd /tmp
wget https://github.com/git/git/archive/v$VERSION.tar.gz

tar -zxf v$VERSION.tar.gz
cd /tmp/git-$VERSION*
# Update NO_GETTEXT to fix the problem of missing libintl in OSX (http://stackoverflow.com/a/11370808/967489):
# CFLAGS and LDFLAGS needs to be updated in OSX (https://www.atlassian.com/git/tutorials/install-git/mac-os-x)
export NO_GETTEXT=1
make CFLAGS="-I/usr/local/opt/openssl/include" LDFLAGS="-L/usr/local/opt/openssl/lib" configure
./configure
make CFLAGS="-I/usr/local/opt/openssl/include" LDFLAGS="-L/usr/local/opt/openssl/lib" all
sudo make CFLAGS="-I/usr/local/opt/openssl/include" LDFLAGS="-L/usr/local/opt/openssl/lib" install
