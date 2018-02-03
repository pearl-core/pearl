#!/usr/bin/env bash
PEARL_LOCATION=$(dirname $0)/../..
source "$PEARL_LOCATION/tests/bunit/utils/utils.sh"
source "$PEARL_LOCATION/tests/test-utils/utils.sh"

source "$PEARL_LOCATION/buava/lib/utils.sh"
source "$PEARL_LOCATION/lib/utils/utils.sh"

# Disable the exiterr
set +e

FILEPATH=/tmp/file_pearl_test

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    pearlHomeSetUp
    touch $FILEPATH
    mkdir -p $HOME/symlinks
    mkdir -p $PEARL_HOME/bin
}

function tearDown(){
    rm $FILEPATH
    pearlHomeTearDown
}

function test_link_to_path_null_executable_path(){
    assertCommandFailOnStatus 11 link_to_path ""
}

function test_link_to_path(){
    echo "Content" > $HOME/binary
    assertCommandSuccess link_to_path "$HOME/binary"
    assertEquals "Content" "$(cat $PEARL_HOME/bin/binary)"
}

function test_link_to_path_new_executable_name(){
    echo "Content" > $HOME/binary
    assertCommandSuccess link_to_path "$HOME/binary" "new_binary"
    assertEquals "Content" "$(cat $PEARL_HOME/bin/new_binary)"
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

function test_unlink_from_path_new_executable_name(){
    echo "Content" > $HOME/binary
    ln -s $HOME/binary $PEARL_HOME/bin/new_binary
    [[ -L "$PEARL_HOME/bin/new_binary" ]]
    assertEquals 0 $?
    assertCommandSuccess unlink_from_path "$HOME/binary" "new_binary"
    [[ ! -L "$PEARL_HOME/bin/new_binary" ]]
    assertEquals 0 $?
}

source $PEARL_LOCATION/tests/bunit/utils/shunit2
