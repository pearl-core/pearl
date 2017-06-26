#!/usr/bin/env bash
PEARL_LOCATION=$(dirname $0)/../..
source "$PEARL_LOCATION/tests/utils/utils.sh"

unset HOME
export HOME=$(TMPDIR=/tmp mktemp -d -t pearl-user-home.XXXXXXX)
unset PEARL_HOME
export PEARL_HOME=$(TMPDIR=/tmp mktemp -d -t pearl-home.XXXXXXX)

source "$PEARL_LOCATION/buava/lib/utils.sh"
source "$PEARL_LOCATION/lib/utils/utils.sh"

# Disable the exiterr
set +e

FILEPATH=/tmp/file_pearl_test

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    touch $FILEPATH
    mkdir -p $HOME/symlinks
    mkdir -p $PEARL_HOME/bin
}

function tearDown(){
    rm $FILEPATH
    rm -rf $HOME
    rm -rf $PEARL_HOME
}

function test_link_to_path_null_executable_path(){
    assertCommandFailOnStatus 11 link_to_path ""
}

function test_link_to_path(){
    echo "Content" > $HOME/binary
    assertCommandSuccess link_to_path "$HOME/binary"
    assertEquals "Content" "$(cat $PEARL_HOME/bin/binary)"
}

function test_unlink_from_path_null_executable_path(){
    assertCommandFailOnStatus 11 unlink_from_path ""
}

function test_unlink_from_path(){
    echo "Content" > $HOME/binary
    ln -s $HOME/binary $PEARL_HOME/bin
    [[ -L "$PEARL_HOME/bin/binary" ]]
    assertEquals 0 $?
    assertCommandSuccess unlink_from_path "$HOME/binary"
    [[ ! -L "$PEARL_HOME/bin/binary" ]]
    assertEquals 0 $?
}

source $(dirname $0)/../utils/shunit2
