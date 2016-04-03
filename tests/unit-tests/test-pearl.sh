#!/bin/bash
source "$(dirname $0)/utils.sh"

PEARL_LOCAL_ROOT="$(dirname $0)/../.."
source $PEARL_LOCAL_ROOT/bin/pearl -h &> /dev/null

# Disable the exiterr
set +e

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    PEARL_ROOT="DEFINE_FOR_SENDING_SIGNAL"
}

function tearDown(){
:
}

## Mock functions ##
KILL_CMD=kill_mock

function kill_mock() {
    echo "kill $@"
}

function usage(){
    echo "usage"
}

function pearl_load_repos(){
    echo "pearl_load_repos"
}

function pearl_install(){
    echo "pearl_install $@"
}

function pearl_update(){
    echo "pearl_update"
}

function pearl_remove(){
    echo "pearl_remove"
}

function pearl_package_install(){
    echo "pearl_package_install $@"
}

function pearl_package_update(){
    echo "pearl_package_update $@"
}

function pearl_package_remove(){
    echo "pearl_package_remove $@"
}

function pearl_package_list(){
    echo "pearl_package_list $@"
}

function pearl_wrap(){
    parse_arguments "$@"
    check_cli
    execute_operation
}

function outputWithKill(){
    echo -e "pearl_load_repos\n$@\nkill -USR1 $PPID"
}

function test_help(){
    assertCommandSuccess pearl_wrap -h
    assertEquals "usage" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap --help
    assertEquals "usage" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap help
    assertEquals "usage" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap h
    assertEquals "usage" "$(cat $STDOUTF)"
}

function test_pearl_install_existing_installation(){
    assertCommandFail pearl_wrap install
    assertEquals "pearl_load_repos" "$(cat $STDOUTF)"
    assertCommandFail pearl_wrap i
    assertEquals "pearl_load_repos" "$(cat $STDOUTF)"
}

function test_pearl_install(){
    unset PEARL_ROOT
    assertCommandSuccess pearl_wrap install
    assertEquals "$(echo -e "pearl_load_repos\npearl_install $PEARL_LOCAL_ROOT")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap i
    assertEquals "$(echo -e "pearl_load_repos\npearl_install $PEARL_LOCAL_ROOT")" "$(cat $STDOUTF)"
}

function test_pearl_update(){
    assertCommandSuccess pearl_wrap update
    assertEquals "$(outputWithKill "pearl_update")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap u
    assertEquals "$(outputWithKill "pearl_update")" "$(cat $STDOUTF)"
}

function test_pearl_remove(){
    assertCommandSuccess pearl_wrap remove
    assertEquals "$(echo -e "pearl_load_repos\npearl_remove")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap r
    assertEquals "$(echo -e "pearl_load_repos\npearl_remove")" "$(cat $STDOUTF)"
}

function test_pearl_package_install(){
    assertCommandSuccess pearl_wrap install vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_install vim/fugitive\npearl_package_install misc/ranger")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap i vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_install vim/fugitive\npearl_package_install misc/ranger")" "$(cat $STDOUTF)"
}

function test_pearl_package_install_already_installed(){
    pearl_package_install(){
        [ "$1" == "misc/ranger" ] && return 1
        echo "pearl_package_install $@"
        return 0
    }
    assertCommandFail pearl_wrap install vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_install vim/fugitive")" "$(cat $STDOUTF)"
    assertCommandFail pearl_wrap i vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_install vim/fugitive")" "$(cat $STDOUTF)"
}

function test_pearl_package_update(){
    assertCommandSuccess pearl_wrap update vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_update vim/fugitive\npearl_package_update misc/ranger")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap u vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_update vim/fugitive\npearl_package_update misc/ranger")" "$(cat $STDOUTF)"
}

function test_pearl_package_update_not_installed(){
    pearl_package_update(){
        [ "$1" == "misc/ranger" ] && return 1
        echo "pearl_package_update $@"
        return 0
    }
    assertCommandFail pearl_wrap update vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_update vim/fugitive")" "$(cat $STDOUTF)"
    assertCommandFail pearl_wrap u vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_update vim/fugitive")" "$(cat $STDOUTF)"
}

function test_pearl_package_remove(){
    assertCommandSuccess pearl_wrap remove vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_remove vim/fugitive\npearl_package_remove misc/ranger")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap r vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_remove vim/fugitive\npearl_package_remove misc/ranger")" "$(cat $STDOUTF)"
}

function test_pearl_package_remove_not_installed(){
    pearl_package_remove(){
        [ "$1" == "misc/ranger" ] && return 1
        echo "pearl_package_remove $@"
        return 0
    }
    assertCommandFail pearl_wrap remove vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_remove vim/fugitive")" "$(cat $STDOUTF)"
    assertCommandFail pearl_wrap r vim/fugitive misc/ranger
    assertEquals "$(outputWithKill "pearl_package_remove vim/fugitive")" "$(cat $STDOUTF)"
}

function test_pearl_package_list(){
    assertCommandSuccess pearl_wrap list
    assertEquals "$(echo -e "pearl_load_repos\npearl_package_list ")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap l
    assertEquals "$(echo -e "pearl_load_repos\npearl_package_list ")" "$(cat $STDOUTF)"
}

function test_pearl_package_search(){
    assertCommandSuccess pearl_wrap search vim
    assertEquals "$(echo -e "pearl_load_repos\npearl_package_list vim")" "$(cat $STDOUTF)"
    assertCommandSuccess pearl_wrap s vim
    assertEquals "$(echo -e "pearl_load_repos\npearl_package_list vim")" "$(cat $STDOUTF)"
}

function test_check_cli(){
    assertCommandFail pearl_wrap
    assertCommandFail pearl_wrap -b
    assertCommandFail pearl_wrap wrong_arg
    assertCommandFail pearl_wrap -h install
}

source $(dirname $0)/shunit2
