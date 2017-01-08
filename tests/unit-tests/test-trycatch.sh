#!/usr/bin/env bash
source "$(dirname $0)/../utils/utils.sh"

source "$(dirname $0)/../../lib/utils/utils.sh"
source "$(dirname $0)/../../lib/utils/trycatch.sh"

# Disable the exiterr
set +e

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    my_func(){
        return 0
    }

}

function tearDown(){
    unset my_func
}

function test_try_null_func() {
    assertCommandFailOnStatus 11 try ""
}

function test_throw_null_func() {
    assertCommandFailOnStatus 11 throw ""
}

function test_try_catch() {
    set -e
    local st=0
    my_func || local st=$?
    assertEquals 0 $st
    set +e

    try my_func
    catch && assertEquals 0 $?
}

function test_try_catch_command_not_found() {
    my_func(){
        not_a_command 2> /dev/null
        throw 99
    }

    set -e
    my_func || local st=$?
    assertEquals 99 $st
    set +e

    try my_func
    catch || assertEquals 1 $?
}

function test_try_catch_keep_error_option() {
    set -e
    try my_func
    catch || { :; }

    [[ $- == *e* ]]
    assertEquals 0 $?

    set +e
    try my_func
    catch || { :; }

    [[ $- == *e* ]]
    assertEquals 1 $?
}

function test_try_catch_throw() {
    my_func(){
        throw 99
        return 0
    }

    set -e
    my_func || local st=$?
    [ -z $st ]
    assertEquals 0 $?
    set +e

    try my_func
    catch || assertEquals 99 $?
}

source $(dirname $0)/../utils/shunit2
