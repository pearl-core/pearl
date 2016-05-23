#!/bin/bash

# Update path for OSX systems
GNUBIN="/usr/local/opt/coreutils/libexec/gnubin"
if [ -d "$GNUBIN" ]; then
    PATH="$GNUBIN:$PATH"
fi

tests_succeded=true
for tst in $(ls $(dirname $0)/test*)
do
    $tst || tests_succeded=false
done

$tests_succeded
