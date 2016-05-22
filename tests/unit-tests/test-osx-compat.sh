#!/bin/bash

source "$(dirname $0)/utils.sh"

source "$(dirname $0)/../../lib/utils/osx-compat.sh"

# Disable the exiterr
set +e

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    GNUBIN=$(TMPDIR=/tmp mktemp -d -t pearl-test-gnubin.XXXXXXX)
}

function tearDown(){
    rm -rf "$GNUBIN"
    unset GNUBIN
}

function test_pearl_update_path_osx_gnubin_no_exists() {
    OLDPATH=$PATH
    GNUBIN="/not-a-directory"
    pearl_update_path_osx
    assertEquals "$OLDPATH" "$PATH"
    PATH=$OLDPATH
}

function test_pearl_update_path_osx() {
    OLDPATH=$PATH
    pearl_update_path_osx
    assertEquals "$GNUBIN:$OLDPATH" "$PATH"
    PATH=$OLDPATH
}

function test_pearl_attempt_command_osx() {
    assertCommandSuccess pearl_attempt_command_osx ls
}

function test_pearl_attempt_command_osx_not_a_command() {
    assertCommandFailOnStatus 127 pearl_attempt_command_osx nocmd
}

function test_pearl_attempt_command_osx_on_gnubin() {
    cat <<EOF > $GNUBIN/mycmd
#!/bin/bash
echo mycommand
EOF
    chmod +x $GNUBIN/mycmd
    assertCommandSuccess pearl_attempt_command_osx mycmd
    assertEquals "mycommand" "$(cat $STDOUTF)"
}

function test_pearl_attempt_command_osx_no_executable() {
    echo "" >> $GNUBIN/mycmd
    assertCommandFailOnStatus 127 pearl_attempt_command_osx mycmd
}

source $(dirname $0)/shunit2
