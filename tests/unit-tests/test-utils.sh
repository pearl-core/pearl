#!/bin/bash
source "$(dirname $0)/utils.sh"

source "$(dirname $0)/../../lib/utils.sh"

# Disable the exiterr
set +e

FILEPATH=/tmp/file_pearl_test

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    touch $FILEPATH
}

function tearDown(){
    rm $FILEPATH
}

function test_echoerr(){
    assertCommandSuccess echoerr "Test"
    assertEquals "Test" "$(cat $STDERRF)"
}

function test_error(){
    assertCommandSuccess error "Test"
    local expected=$(echo -e "\033[1;31mTest\033[0m")
    assertEquals "$expected" "$(cat $STDERRF)"
}

function test_warn(){
    assertCommandSuccess warn "Test"
    local expected=$(echo -e "\033[1;33mTest\033[0m")
    assertEquals "$expected" "$(cat $STDERRF)"
}

function test_info(){
    assertCommandSuccess info "Test"
    local expected=$(echo -e "\033[1;37mTest\033[0m")
    assertEquals "$expected" "$(cat $STDOUTF)"
}

function test_die(){
    assertCommandFail die "Test"
    local expected=$(echo -e "\033[1;31mTest\033[0m")
    assertEquals "$expected" "$(cat $STDERRF)"
}

function test_ask(){
    echo "Y" | ask "Test" &> /dev/null
    assertEquals $? 0
    echo "y" | ask "Test" &> /dev/null
    assertEquals $? 0
    echo "N" | ask "Test" &> /dev/null
    assertEquals $? 1
    echo "n" | ask "Test" &> /dev/null
    assertEquals $? 1
    echo -e "\n" | ask "Test" &> /dev/null
    assertEquals $? 0
    echo -e "\n" | ask "Test" "N" &> /dev/null
    assertEquals $? 1
    echo -e "asdf\n\n" | ask "Test" "N" &> /dev/null
    assertEquals $? 1
}

function test_apply_at_beginning(){
    echo -e "myoldstring\nmynewstring" > $FILEPATH
    assertCommandSuccess apply "mystring" $FILEPATH
    assertEquals "$(echo -e "mystring\nmyoldstring\nmynewstring")" "$(cat $FILEPATH)"

    echo -e "myoldstring\nmynewstring" > $FILEPATH
    assertCommandSuccess apply "mystring" $FILEPATH true
    assertEquals "$(echo -e "mystring\nmyoldstring\nmynewstring")" "$(cat $FILEPATH)"
}

function test_apply_at_end(){
    echo -e "myoldstring\nmynewstring" > $FILEPATH
    assertCommandSuccess apply "mystring" $FILEPATH false
    assertEquals "$(echo -e "myoldstring\nmynewstring\nmystring")" "$(cat $FILEPATH)"
}

function test_apply_create_directory(){
    local filepath=/tmp/mydir/myfile
    assertCommandSuccess apply "mystring" $filepath false
    assertEquals "$(echo -e "\nmystring")" "$(cat $filepath)"

    rm $filepath
    rmdir $(dirname $filepath)
}

function test_is_not_applied(){
    assertCommandFailOnStatus 1 is_applied "mystring" $FILEPATH
}

function test_is_applied_file_not_exist(){
    assertCommandFailOnStatus 2 is_applied "mystring" /tmp/file-does-not-exist
}

function test_is_applied(){
    echo -e "myoldstring\nmystring\nmynewstring" > $FILEPATH
    assertCommandSuccess is_applied "mystring" $FILEPATH
}

function test_unapply_on_empty_file(){
    assertCommandSuccess unapply "mystring" $FILEPATH
    assertEquals "" "$(cat $FILEPATH)"
}

function test_unapply_on_non_existing_file(){
    assertCommandSuccess unapply "mystring" "${FILEPATH}_no_existing"
    [ ! -e "${FILEPATH}_no_existing" ]
    assertEquals 0 $?
}

function test_unapply_with_match(){
    echo -e "myoldstring\nmystring\nmynewstring" > $FILEPATH
    assertCommandSuccess unapply "mystring" $FILEPATH
    assertEquals "$(echo -e "myoldstring\nmynewstring")" "$(cat $FILEPATH)"
}
function test_unapply_with_a_complex_match(){
    echo -e "myoldstring\nmy(s.*t\\\[ri[ng\nmynewstring" > $FILEPATH
    assertCommandSuccess unapply "my(s.*t\[ri[ng" $FILEPATH
    assertEquals "$(echo -e "myoldstring\nmynewstring")" "$(cat $FILEPATH)"
}

function test_unapply_without_match(){
    echo -e "myoldstring\nmystring\nmynewstring" > $FILEPATH
    assertCommandSuccess unapply "mystring2" $FILEPATH
    assertEquals "$(echo -e "myoldstring\nmystring\nmynewstring")" "$(cat $FILEPATH)"
}

source $(dirname $0)/shunit2
