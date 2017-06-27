#!/usr/bin/env bash
PEARL_LOCATION=$(dirname $0)/../..
source "$PEARL_LOCATION/tests/bunit/utils/utils.sh"
source "$PEARL_LOCATION/tests/test-utils/utils.sh"

source "$PEARL_LOCATION/buava/lib/utils.sh"
source "$PEARL_LOCATION/lib/core/package.sh"
source "$PEARL_LOCATION/lib/core/system.sh"

# Disable the exiterr
set +e

declare -A CONFIG_FILES
declare -a RESULT

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    pearlSetUp

    # The following ensure to override CONFIG_FILES with the right HOME variable
    CONFIG_FILES[bash]="$HOME/.bashrc"
    CONFIG_FILES[emacs]="$HOME/.emacs"
    CONFIG_FILES[fish]="$HOME/.config/fish/config.fish"
    CONFIG_FILES[vim]="$HOME/.vimrc"
    CONFIG_FILES[zsh]="$HOME/.zshrc"
}

function tearDown(){
    pearlTearDown
}

function get_list_installed_packages(){
    RESULT+=("ls-colors")
}

function pearl_package_update(){
    echo "updating: $@"
}

function pearl_package_remove(){
    echo "removing: $@"
}

function test_pearl_init(){
    rm -rf $PEARL_HOME
    assertCommandSuccess pearl_init
    cat $STDERRF

    [ -d $PEARL_HOME/bin ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/packages ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/repos ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/tmp ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var ]
    assertEquals 0 $?

    [ -e $PEARL_HOME/pearl.conf ]
    assertEquals 0 $?

    assertEquals "$(readlink -f $PEARL_HOME/bin/pearl)" "$(readlink -f $PEARL_ROOT/bin/pearl)"

    assertEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.bashrc)"
    assertEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.zshrc)"
    assertEquals "$(echo -e "set -x PEARL_ROOT $PEARL_ROOT\nsource ${PEARL_ROOT}/boot/fish/pearl.fish")" "$(cat $HOME/.config/fish/config.fish)"
    assertEquals "$(echo -e "source ${PEARL_ROOT}/boot/vim/pearl.vim")" "$(cat $HOME/.vimrc)"
    assertEquals "$(echo -e "(load-file \"${PEARL_ROOT}/boot/emacs/pearl.el\")")" "$(cat $HOME/.emacs)"
}

function test_pearl_update(){
    git_mock(){
        :
    }
    GIT=git_mock
    assertCommandSuccess pearl_update
    cat $STDOUTF | grep -q "updating: ls-colors"
    assertEquals 0 $?
}

function test_pearl_remove(){
    ask(){
        return 0
    }
    git_mock(){
        :
    }
    GIT=git_mock
    echo "export PEARL_ROOT=${PEARL_ROOT}" > ${HOME}/.bashrc
    echo "source ${PEARL_ROOT}/boot/sh/pearl.sh" >> ${HOME}/.bashrc
    echo "export PEARL_ROOT=${PEARL_ROOT}" > ${HOME}/.zshrc
    echo "source ${PEARL_ROOT}/boot/sh/pearl.sh" >> ${HOME}/.zshrc
    mkdir -p ${HOME}/.config/fish
    echo "set -x PEARL_ROOT ${PEARL_ROOT}" > ${HOME}/.config/fish/config.fish
    echo "source ${PEARL_ROOT}/boot/sh/pearl.fish" >> ${HOME}/.config/fish/config.fish
    echo "source ${PEARL_ROOT}/boot/vim/pearl.vim" > ${HOME}/.vimrc
    echo "(load-file \"${PEARL_ROOT}/boot/emacs/pearl.el\")" > ${HOME}/.emacs

    assertCommandSuccess pearl_remove
    [ ! -e $PEARL_HOME ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "removing: ls-colors"
    assertEquals 0 $?

    assertNotEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.bashrc)"
    assertNotEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.zshrc)"
    assertNotEquals "$(echo -e "set -x PEARL_ROOT $PEARL_ROOT\nsource ${PEARL_ROOT}/boot/fish/pearl.fish")" "$(cat $HOME/.config/fish/config.fish)"
    assertNotEquals "$(echo -e "source ${PEARL_ROOT}/boot/vim/pearl.vim")" "$(cat $HOME/.vimrc)"
    assertNotEquals "$(echo -e "(load-file \"${PEARL_ROOT}/boot/emacs/pearl.el\")")" "$(cat $HOME/.emacs)"
}

function test_pearl_remove_no(){
    ask(){
        return 1
    }
    git_mock(){
        # This should never happen
        assertTrue "The git command has been executed" 123
    }
    GIT=git_mock
    echo "export PEARL_ROOT=${PEARL_ROOT}" > ${HOME}/.bashrc
    echo "source ${PEARL_ROOT}/boot/sh/pearl.sh" >> ${HOME}/.bashrc
    echo "export PEARL_ROOT=${PEARL_ROOT}" > ${HOME}/.zshrc
    echo "source ${PEARL_ROOT}/boot/sh/pearl.sh" >> ${HOME}/.zshrc
    mkdir -p ${HOME}/.config/fish
    echo "set -x PEARL_ROOT ${PEARL_ROOT}" > ${HOME}/.config/fish/config.fish
    echo "source ${PEARL_ROOT}/boot/fish/pearl.fish" >> ${HOME}/.config/fish/config.fish
    echo "source ${PEARL_ROOT}/boot/vim/pearl.vim" > ${HOME}/.vimrc
    echo "(load-file \"${PEARL_ROOT}/boot/emacs/pearl.el\")" > ${HOME}/.emacs

    assertCommandSuccess pearl_remove

    [ -e $PEARL_HOME ]
    assertEquals 0 $?

    assertEquals "" "$(cat $STDOUTF)"

    assertEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.bashrc)"
    assertEquals "$(echo -e "export PEARL_ROOT=$PEARL_ROOT\nsource ${PEARL_ROOT}/boot/sh/pearl.sh")" "$(cat $HOME/.zshrc)"
    assertEquals "$(echo -e "set -x PEARL_ROOT $PEARL_ROOT\nsource ${PEARL_ROOT}/boot/fish/pearl.fish")" "$(cat $HOME/.config/fish/config.fish)"
    assertEquals "$(echo -e "source ${PEARL_ROOT}/boot/vim/pearl.vim")" "$(cat $HOME/.vimrc)"
    assertEquals "$(echo -e "(load-file \"${PEARL_ROOT}/boot/emacs/pearl.el\")")" "$(cat $HOME/.emacs)"
}

source $PEARL_LOCATION/tests/bunit/utils/shunit2
