#!/bin/bash
source "$(dirname $0)/utils.sh"

source "$(dirname $0)/../../lib/utils.sh"
source "$(dirname $0)/../../lib/core/package.sh"

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
    unset GIT
}

function scenario_misc_mods(){
    mkdir -p $PEARL_HOME/packages/default/pearl-ssh/.git
    mkdir -p $PEARL_HOME/packages/default/ls-colors/.git
    mkdir -p $PEARL_HOME/packages/default/vim-rails/.git
    mkdir -p $PEARL_HOME/packages/default/pearl-ssh/pearl-metadata
    mkdir -p $PEARL_HOME/packages/default/ls-colors/pearl-metadata
    mkdir -p $PEARL_HOME/packages/default/vim-rails/pearl-metadata
    create_pearl_conf
    create_install "pearl-ssh" post_install \
                               pre_update post_update \
                               pre_remove post_remove
    create_install "ls-colors" post_install \
                               pre_update post_update \
                               pre_remove post_remove
    git_mock(){
        :
    }
    GIT=git_mock
}

function create_pearl_conf(){
    local pearl_conf_content=$(cat <<EOF
PEARL_PACKAGES[pearl-utils]="https://pearl-utils"
PEARL_PACKAGES[pearl-ssh]="https://pearl-ssh"
PEARL_PACKAGES[ls-colors]="https://ls-colors"
EOF
)
    echo "$pearl_conf_content" > $PEARL_HOME/pearl.conf
}

function create_install(){
    local pkgname=$1
    shift
    local install_content=""
    for hookfunc in $@
    do
        local content=$(cat <<EOF
function $hookfunc(){
    if [ "$hookfunc" == "post_remove" ]
    then
        [ \$PEARL_HOME != \${PWD} ] && return 1
    else
        [ \$PEARL_PKGDIR != \${PWD} ] && return 1
    fi
    echo "$hookfunc"
}
EOF
)
        install_content=$(echo -e "$install_content\n$content")
    done
    echo "$install_content" > $PEARL_HOME/packages/default/${pkgname}/pearl-metadata/install.sh
}

function create_bad_install(){
    local pkgname=$1
    local hookfunc=$2
    local install_content=$(cat <<EOF
function $hookfunc(){
    echo "$hookfunc"
    return 1
}
EOF
)
    echo "$install_content" > $PEARL_HOME/packages/default/${pkgname}/pearl-metadata/install.sh
}

function load_repo_first() {
    pearl_load_repos
    $@
}

function test_pearl_package_list_empty_pattern(){
    scenario_misc_mods
    assertCommandSuccess load_repo_first pearl_package_list ""
    cat $STDOUTF | grep -qE "pearl-utils"
    assertEquals 0 $?
    cat $STDOUTF | grep -qE "pearl-ssh.*[installed]"
    assertEquals 0 $?
    cat $STDOUTF | grep -qE "ls-colors.*[installed]"
    assertEquals 0 $?
}

function test_pearl_module_list_matching(){
    scenario_misc_mods
    assertCommandSuccess load_repo_first pearl_package_list "pearl"
    cat $STDOUTF | grep -qE "pearl-utils"
    assertEquals 0 $?
    cat $STDOUTF | grep -qE "pearl-ssh.*[installed]"
    assertEquals 0 $?
    cat $STDOUTF | grep -qEv "ls-colors.*[installed]"
    assertEquals 0 $?
}

function test_pearl_module_list_not_matching(){
    scenario_misc_mods
    assertCommandSuccess pearl_package_list "blahblah"
    cat $STDOUTF | grep -qE "pearl-utils"
    assertEquals 1 $?
    cat $STDOUTF | grep -qE "pearl-ssh .*[installed]"
    assertEquals 1 $?
    cat $STDOUTF | grep -qE "ls-colors .*[installed]"
    assertEquals 1 $?
}

function test_pearl_package_install(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
        mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
        create_install "$pkgname" post_install
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 0 $?
}

function test_pearl_package_install_errors_on_hooks(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
        mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
        create_bad_install $pkgname post_install
    }
    GIT=git_mock

    assertCommandFailOnStatus 4 load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 0 $?
}

function test_pearl_package_install_deinit(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
        mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
        create_install "$pkgname" post_install
    }
    GIT=git_mock
    # Check if unset of hooks works
    load_repo_first pearl_package_install "$pkgname" > /dev/null
    type -t post_install
    assertEquals 1 $?
}

function test_pearl_package_install_already_installed(){
    local pkgname="ls-colors"
    scenario_misc_mods
    assertCommandFailOnStatus 1 load_repo_first pearl_package_install "$pkgname"
}

function test_pearl_package_install_no_install_file(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
        mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 1 $?
}

function test_pearl_package_install_empty_install(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
        mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
        echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    assertEquals "" "$(cat "$STDOUTF" | grep -v "Installing")"
}

function test_pearl_package_install_not_existing_package(){
    local pkgname="blahblah"
    scenario_misc_mods
    assertCommandFailOnStatus 2 load_repo_first pearl_package_install "$pkgname"
}

function test_pearl_package_remove(){
    local pkgname="ls-colors"
    scenario_misc_mods
    assertCommandSuccess load_repo_first pearl_package_remove "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "pre_remove"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_remove"
    assertEquals 0 $?
}

function test_pearl_package_remove_errors_on_hooks(){
    local pkgname="ls-colors"
    scenario_misc_mods
    create_bad_install $pkgname pre_remove
    assertCommandFailOnStatus 3 load_repo_first pearl_package_remove "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "pre_remove"
    assertEquals 0 $?

    create_bad_install $pkgname post_remove
    assertCommandFailOnStatus 4 load_repo_first pearl_package_remove "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_remove"
    assertEquals 0 $?
}

function test_pearl_package_remove_deinit(){
    local pkgname="ls-colors"
    scenario_misc_mods
    # Check if unset of hooks works
    load_repo_first pearl_package_remove "$pkgname" > /dev/null
    type -t pre_remove
    assertEquals 1 $?
    type -t post_remove
    assertEquals 1 $?
}

function test_pearl_package_remove_not_installed(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    assertCommandFailOnStatus 1 load_repo_first pearl_package_remove $pkgname
}

function test_pearl_package_remove_empty_install(){
    local pkgname="ls-colors"
    scenario_misc_mods
    echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    assertCommandSuccess load_repo_first pearl_package_remove $pkgname
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    assertEquals "" "$(cat "$STDOUTF" | grep -v "Removing")"
}

function test_pearl_package_remove_not_existing_package(){
    local pkgname="ls-colors"
    scenario_misc_mods
    unset PEARL_PACKAGES[ls-colors]
    assertCommandSuccess load_repo_first pearl_package_remove "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
}

function test_pearl_package_update(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        echo "git pull"
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "pre_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git pull"
    assertEquals 0 $?
}

function test_pearl_package_update_errors_on_hooks(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        echo "git pull"
    }
    GIT=git_mock

    create_bad_install $pkgname pre_update
    assertCommandFailOnStatus 3 load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "pre_update"
    assertEquals 0 $?

    create_bad_install $pkgname post_update
    assertCommandFailOnStatus 4 load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_update"
    assertEquals 0 $?
}

function test_pearl_package_update_deinit(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        echo "git pull"
    }
    GIT=git_mock
    # Check if unset of hooks works
    load_repo_first pearl_package_update "$pkgname" > /dev/null
    type -t pre_update
    assertEquals 1 $?
    type -t post_update
    assertEquals 1 $?
}

function test_pearl_package_update_not_installed(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    assertCommandFailOnStatus 1 load_repo_first pearl_package_update $pkgname
}

function test_pearl_package_update_empty_install(){
    local pkgname="ls-colors"
    scenario_misc_mods
    echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    assertCommandSuccess load_repo_first pearl_package_update $pkgname
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    assertEquals "" "$(cat "$STDOUTF" | grep -v "Updating")"
}

function test_pearl_package_update_not_existing_package(){
    local pkgname="vim-rails"
    scenario_misc_mods
    assertCommandFailOnStatus 2 load_repo_first pearl_package_update "$pkgname"
}

function test_pearl_load_repos(){
    scenario_misc_mods
    git_mock() {
        assertEquals "$PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2" $PWD
        echo $@
    }
    GIT=git_mock
    mkdir -p $PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2/.git
    echo "PEARL_REPOS+=(\"https://pearl-repo.git\")" > $PEARL_HOME/pearl.conf
    echo "echo \"sourced repo.conf\"" > $PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2/repo.conf
    assertCommandSuccess pearl_load_repos
    assertEquals \
        "$(echo -e "pull --quiet origin master\nsourced repo.conf")" "$(cat $STDOUTF | grep -v "Updating")"
}

function test_pearl_load_repos_new(){
    scenario_misc_mods
    echo "PEARL_REPOS+=(\"https://pearl-repo.git\")" > $PEARL_HOME/pearl.conf
    git_mock() {
        echo $@
        mkdir -p $PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2/.git
        echo "echo \"sourced repo.conf\"" > $PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2/repo.conf
    }
    GIT=git_mock
    assertCommandSuccess pearl_load_repos
    assertEquals \
        "$(echo -e "clone --quiet --depth 1 https://pearl-repo.git $PEARL_HOME/repos/31c8bf07d0de14c822e9f085156aeca2\nsourced repo.conf")" "$(cat $STDOUTF | grep -v Initializing)"
}

source $(dirname $0)/shunit2
