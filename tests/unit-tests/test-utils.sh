#!/bin/bash
source "$(dirname $0)/utils.sh"

unset HOME
export HOME=$(TMPDIR=/tmp mktemp -d -t pearl-user-home.XXXXXXX)

source "$(dirname $0)/../../lib/utils/utils.sh"

# Disable the exiterr
set +e

FILEPATH=/tmp/file_pearl_test

function oneTimeSetUp(){
    setUpUnitTests
}

function setUp(){
    touch $FILEPATH
    mkdir -p $HOME
}

function tearDown(){
    rm $FILEPATH
    rm -rf $HOME
}

function test_check_not_null(){
    assertCommandFailOnStatus 11 check_not_null "" ""
    assertCommandSuccess check_not_null "bla" ""
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
    local expected=$(echo -e "\033[1;36mTest\033[0m")
    assertEquals "$expected" "$(cat $STDOUTF)"
}

function test_die(){
    assertCommandFail die "Test"
    local expected=$(echo -e "\033[1;31mTest\033[0m")
    assertEquals "$expected" "$(cat $STDERRF)"
}

function test_die_on_status(){
    assertCommandFailOnStatus 222 die_on_status 222 "Test"
    local expected=$(echo -e "\033[1;31mTest\033[0m")
    assertEquals "$expected" "$(cat $STDERRF)"
}

function test_ask_null_question(){
    assertCommandFailOnStatus 11 ask "" "Y"
}

function test_ask(){
    echo "Y" | ask "Test" &> /dev/null
    assertEquals 0 $?
    echo "y" | ask "Test" &> /dev/null
    assertEquals 0 $?
    echo "N" | ask "Test" &> /dev/null
    assertEquals 1 $?
    echo "n" | ask "Test" &> /dev/null
    assertEquals 1 $?
    echo -e "\n" | ask "Test" &> /dev/null
    assertEquals 0 $?
    echo -e "\n" | ask "Test" "N" &> /dev/null
    assertEquals 1 $?
    echo -e "asdf\n\n" | ask "Test" "N" &> /dev/null
    assertEquals 1 $?
}

function test_ask_wrong_default_answer() {
    echo "Y" | ask "Test" G &> /dev/null
    assertEquals 33 $?
}

function test_apply_null_line(){
    assertCommandFailOnStatus 11 apply "" "$FILEPATH"
}

function test_apply_null_filepath(){
    assertCommandFailOnStatus 11 apply "source blah" ""
}

function test_apply_at_top(){
    echo -e "myoldstring\nmynewstring" > $FILEPATH
    assertCommandSuccess apply "mystring" $FILEPATH
    assertEquals "$(echo -e "mystring\nmyoldstring\nmynewstring")" "$(cat $FILEPATH)"

    echo -e "myoldstring\nmynewstring" > $FILEPATH
    assertCommandSuccess apply "mystring" $FILEPATH true
    assertEquals "$(echo -e "mystring\nmyoldstring\nmynewstring")" "$(cat $FILEPATH)"
}

function test_apply_file_with_spaces(){
    local filepath="/tmp/myfile with spaces"
    echo -e "myoldstring" > "$filepath"
    assertCommandSuccess apply "mystring" "$filepath"
    assertEquals "$(echo -e "mystring\nmyoldstring")" "$(cat "$filepath")"
}

function test_apply_at_bottom(){
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

function test_is_applied_null_line(){
    assertCommandFailOnStatus 11 is_applied "" "$FILEPATH"
}

function test_is_applied_null_filepath(){
    assertCommandFailOnStatus 11 is_applied "source blah" ""
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

function test_unapply_null_line(){
    assertCommandFailOnStatus 11 unapply "" "$FILEPATH"
}

function test_unapply_null_filepath(){
    assertCommandFailOnStatus 11 unapply "source blah" ""
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

function test_link_null_program(){
    assertCommandFailOnStatus 11 link "" "$FILEPATH"
}

function test_link_null_filepath(){
    assertCommandFailOnStatus 11 link "vim" ""
}

function test_link_at_top(){
    echo -e "myoldstring" > $HOME/.vimrc
    assertCommandSuccess link "vim" $FILEPATH
    assertEquals "$(echo -e "source $FILEPATH\nmyoldstring")" "$(cat $HOME/.vimrc)"

    echo -e "myoldstring" > $HOME/.vimrc
    assertCommandSuccess link "vim" $FILEPATH true
    assertEquals "$(echo -e "source $FILEPATH\nmyoldstring")" "$(cat $HOME/.vimrc)"
}

function test_link_at_bottom(){
    echo -e "myoldstring" > $HOME/.vimrc
    assertCommandSuccess link "vim" $FILEPATH false
    assertEquals "$(echo -e "myoldstring\nsource $FILEPATH")" "$(cat $HOME/.vimrc)"
}

function test_link_not_a_program(){
    assertCommandFailOnStatus 33 link "notvim" $FILEPATH
}

function test_link_file_with_spaces(){
    local filepath="/tmp/myfile with spaces"
    touch $HOME/.bashrc
    echo 'p="pwd"' > "$filepath"
    assertCommandSuccess link "bash" "$filepath"
    assertEquals "$(echo -e "source \"$filepath\"")" "$(cat $HOME/.bashrc)"
    assertEquals "pwd" "$(bash -c "source $HOME/.bashrc; echo \$p")"
}

function test_link_all_programs(){
    assertCommandSuccess link bash $FILEPATH
    assertEquals "$(echo -e "source \"$FILEPATH\"")" "$(cat $HOME/.bashrc)"
    assertCommandSuccess unlink bash $FILEPATH
    assertEquals "" "$(cat $HOME/.bashrc)"

    assertCommandSuccess link emacs $FILEPATH
    assertEquals "$(echo -e "(load-file \"$FILEPATH\")")" "$(cat $HOME/.emacs)"
    assertCommandSuccess unlink emacs $FILEPATH
    assertEquals "" "$(cat $HOME/.emacs)"

    assertCommandSuccess link fish $FILEPATH
    assertEquals "$(echo -e "source \"$FILEPATH\"")" "$(cat $HOME/.config/fish/config.fish)"
    assertCommandSuccess unlink fish $FILEPATH
    assertEquals "" "$(cat $HOME/.config/fish/config.fish)"

    assertCommandSuccess link git $FILEPATH
    assertEquals "$(echo -e "[include] path = \"$FILEPATH\"")" "$(cat $HOME/.gitconfig)"
    assertCommandSuccess unlink git $FILEPATH
    assertEquals "" "$(cat $HOME/.gitconfig)"

    assertCommandSuccess link inputrc $FILEPATH
    assertEquals "$(echo -e "\$include $FILEPATH")" "$(cat $HOME/.inputrc)"
    assertCommandSuccess unlink inputrc $FILEPATH
    assertEquals "" "$(cat $HOME/.inputrc)"

    assertCommandSuccess link mutt $FILEPATH
    assertEquals "$(echo -e "source $FILEPATH")" "$(cat $HOME/.muttrc)"
    assertCommandSuccess unlink mutt $FILEPATH
    assertEquals "" "$(cat $HOME/.muttrc)"

    assertCommandSuccess link screen $FILEPATH
    assertEquals "$(echo -e "source $FILEPATH")" "$(cat $HOME/.screenrc)"
    assertCommandSuccess unlink screen $FILEPATH
    assertEquals "" "$(cat $HOME/.screenrc)"

    assertCommandSuccess link tmux $FILEPATH
    assertEquals "$(echo -e "source $FILEPATH")" "$(cat $HOME/.tmux.conf)"
    assertCommandSuccess unlink tmux $FILEPATH
    assertEquals "" "$(cat $HOME/.tmux.conf)"

    assertCommandSuccess link "vim" $FILEPATH
    assertEquals "$(echo -e "source $FILEPATH")" "$(cat $HOME/.vimrc)"
    assertCommandSuccess unlink vim $FILEPATH
    assertEquals "" "$(cat $HOME/.vimrc)"

    assertCommandSuccess link zsh $FILEPATH
    assertEquals "$(echo -e "source \"$FILEPATH\"")" "$(cat $HOME/.zshrc)"
    assertCommandSuccess unlink zsh $FILEPATH
    assertEquals "" "$(cat $HOME/.zshrc)"
}

function test_unlink_null_program(){
    assertCommandFailOnStatus 11 unlink "" "$FILEPATH"
}

function test_unlink_null_filepath(){
    assertCommandFailOnStatus 11 unlink "vim" ""
}

function test_unlink(){
    echo -e "myoldstring\nsource $FILEPATH" > $HOME/.vimrc
    assertCommandSuccess unlink "vim" $FILEPATH
    assertEquals "$(echo -e "myoldstring")" "$(cat $HOME/.vimrc)"
}

function test_unlink_not_a_program(){
    assertCommandFailOnStatus 33 unlink "notvim" $FILEPATH
}

source $(dirname $0)/shunit2
