#!/bin/bash
source "$(dirname $0)/../utils/utils.sh"

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

function fish_wrapper(){
    echo "$@" > "${OUTPUT_DIR}/fish_command"
    fish ${OUTPUT_DIR}/fish_command
}

function test_pearl_update_path_gnubin_no_exists() {
    assertCommandSuccess fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN 'not-a-directory'; pearl_update_path; echo \$PATH"
    assertEquals "$(fish_wrapper "echo \$PATH")" "$(cat $STDOUTF)"
}

function test_pearl_update_path() {
    assertCommandSuccess fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN '$GNUBIN'; pearl_update_path; echo \$PATH"
    assertEquals "$(fish_wrapper "echo $GNUBIN \$PATH")" "$(cat $STDOUTF)"
}

function test_pearl_attempt_command() {
    assertCommandSuccess fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN '$GNUBIN'; pearl_attempt_command ls"
}

function test_pearl_attempt_command_not_a_command() {
    assertCommandFailOnStatus 127 fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN '$GNUBIN'; pearl_attempt_command nocmd"
}

function test_pearl_attempt_command_on_gnubin() {
    cat <<EOF > $GNUBIN/mycmd
#!/bin/bash
echo mycommand
EOF
    chmod +x $GNUBIN/mycmd
    assertCommandSuccess fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN '$GNUBIN'; pearl_attempt_command mycmd"
    assertEquals "mycommand" "$(cat $STDOUTF)"
    cat $STDERRF
}

function test_pearl_attempt_command_no_executable() {
    echo "" >> $GNUBIN/mycmd
    assertCommandFailOnStatus 127 fish_wrapper "source $(dirname $0)/../../lib/utils/osx-compat.fish; set GNUBIN '$GNUBIN'; pearl_attempt_command mycmd"
}

source $(dirname $0)/../utils/shunit2
