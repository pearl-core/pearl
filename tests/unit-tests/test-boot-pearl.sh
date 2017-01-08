#!/usr/bin/env bash
source "$(dirname $0)/../utils/utils.sh"

# Disable the exiterr
set +e

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    pearlSetUp
}

function tearDown(){
    pearlTearDown
}

function ls_colors_scenario() {
    mkdir -p ${PEARL_HOME}/packages/pearl/ls-colors/pearl-metadata
    create_config_file config.sh
    create_config_file config.bash
    create_config_file config.zsh
    create_config_file config.fish
}

function create_config_file() {
    local configfile=$1
    local content=$(cat <<EOF
    echo sourced $configfile;
    echo \$PEARL_PKGDIR
    echo \$PEARL_PKGVARDIR
EOF
)
    echo "$content" > ${PEARL_HOME}/packages/pearl/ls-colors/pearl-metadata/$configfile
}

function fish_wrapper(){
    echo "$@" > "${OUTPUT_DIR}/fish_command"
    fish ${OUTPUT_DIR}/fish_command
}

function bash_wrapper(){
    $@
    source "${OUTPUT_DIR}/sourced_file"
}

function test_pearl_no_pearl_root_var(){
    OLD_PEARL_ROOT=$PEARL_ROOT
    unset PEARL_ROOT
    assertCommandFailOnStatus 1 source $(dirname $0)/../../boot/sh/pearl.sh
    assertCommandFailOnStatus 1 fish_wrapper "source $(dirname $0)/../../boot/fish/pearl.fish"
    PEARL_ROOT=$OLD_PEARL_ROOT
}

function test_pearl_wrong_pearl_root_var(){
    OLD_PEARL_ROOT=$PEARL_ROOT
    export PEARL_ROOT="/tmmmmp"
    assertCommandFailOnStatus 2 source $(dirname $0)/../../boot/sh/pearl.sh
    assertCommandFailOnStatus 2 fish_wrapper "source $(dirname $0)/../../boot/fish/pearl.fish"
    PEARL_ROOT=$OLD_PEARL_ROOT
}

function test_pearl(){
    ls_colors_scenario
    local test_content=$(cat <<EOF
    # Make sure that PEARL_HOME, PEARL_ROOT, PEARL_TEMPORARY are set with export
    env | grep -q PEARL_HOME
    env | grep -q PEARL_ROOT
    env | grep -q PEARL_TEMPORARY
    echo \$PATH | grep -q \$PEARL_HOME/bin
    echo \$MANPATH | grep -q \$PEARL_ROOT/man
EOF
)
    echo -e "$test_content" > ${OUTPUT_DIR}/sourced_file

    ZSH_NAME="SOMENAME"
    BASH="SOMENAME"
    assertCommandSuccess bash_wrapper source $(dirname $0)/../../boot/sh/pearl.sh
    assertEquals "$(echo -e "sourced osx-compat.sh\nsourced config.sh\n$PEARL_HOME/packages/pearl/ls-colors\n$PEARL_HOME/var/pearl/ls-colors\nsourced config.bash\n$PEARL_HOME/packages/pearl/ls-colors\n$PEARL_HOME/var/pearl/ls-colors\nsourced config.zsh\n$PEARL_HOME/packages/pearl/ls-colors\n$PEARL_HOME/var/pearl/ls-colors")" "$(cat $STDOUTF)"
}

function test_pearl_add_path_once() {
    local test_content=$(cat <<EOF
    echo \$PATH | tr ":" "\n" | grep \$PEARL_HOME/bin | wc -l
    echo \$MANPATH | tr ":" "\n" | grep \$PEARL_ROOT/man | wc -l
EOF
)
    echo -e "$test_content" > ${OUTPUT_DIR}/sourced_file

    PATH=$PATH:$PEARL_HOME/bin
    MANPATH=$MANPATH:$PEARL_ROOT/man
    assertCommandSuccess bash_wrapper source $(dirname $0)/../../boot/sh/pearl.sh
    assertEquals "$(echo -e "sourced osx-compat.sh\n1\n1")" "$(cat $STDOUTF)"
}

function test_pearl_fish(){
    ls_colors_scenario
    local test_content=$(cat <<EOF
    source $(dirname $0)/../../boot/fish/pearl.fish;
    env | grep -q PEARL_HOME;
    env | grep -q PEARL_ROOT;
    env | grep -q PEARL_TEMPORARY;
    echo \$PATH | grep -q \$PEARL_HOME/bin;
    echo \$MANPATH | grep -q \$PEARL_ROOT/man;
EOF
)
    assertCommandSuccess fish_wrapper "$test_content"
    assertEquals "$(echo -e "sourced config.fish\n$PEARL_HOME/packages/pearl/ls-colors\n$PEARL_HOME/var/pearl/ls-colors")" "$(cat $STDOUTF)"
}

function test_pearl_fish_add_path_once() {
    local test_content=$(cat <<EOF
    source $(dirname $0)/../../boot/fish/pearl.fish;
    echo \$PATH | tr " " "\n" | grep \$PEARL_HOME/bin | wc -l
    echo \$MANPATH | tr " " "\n" | grep \$PEARL_ROOT/man | wc -l
EOF
)
    PATH=$PATH:$PEARL_HOME/bin
    MANPATH=$MANPATH:$PEARL_ROOT/man
    assertCommandSuccess fish_wrapper "$test_content"
    assertEquals "$(echo -e "1\n1")" "$(cat $STDOUTF)"
}

function test_pearl_config_error(){
    ls_colors_scenario
    echo "return 123" > ${PEARL_HOME}/packages/pearl/ls-colors/pearl-metadata/config.sh
    assertCommandFailOnStatus 123 source $(dirname $0)/../../boot/sh/pearl.sh
}

function test_pearl_fish_config_error(){
    ls_colors_scenario
    # Unfortunately there is not an equivalent to the bash 'set -e'
    # Fish will return 0 in this case
    # (more info: https://github.com/fish-shell/fish-shell/issues/805)
    echo "return 123" > ${PEARL_HOME}/packages/pearl/ls-colors/pearl-metadata/config.fish
    assertCommandSuccess fish_wrapper "source $(dirname $0)/../../boot/fish/pearl.fish"
}

source $(dirname $0)/../utils/shunit2
