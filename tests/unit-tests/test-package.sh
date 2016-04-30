#!/bin/bash
source "$(dirname $0)/utils.sh"

source "$(dirname $0)/../../lib/utils/utils.sh"
source "$(dirname $0)/../../lib/utils/trycatch.sh"
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
    create_package pearl-ssh
    create_package ls-colors
    create_package vim-rails
    create_pearl_conf "pearl-utils" "https://pearl-utils" \
                      "pearl-ssh" "https://pearl-ssh" \
                      "ls-colors" "https://ls-colors"
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

function create_package(){
    local pkgname=$1
    mkdir -p $PEARL_HOME/packages/default/$pkgname/.git
    mkdir -p $PEARL_HOME/packages/default/$pkgname/pearl-metadata
    mkdir -p $PEARL_HOME/var/default/$pkgname
}

function scenario_local_pkgs(){
    create_package vim-django
    touch $PEARL_HOME/packages/default/vim-django/file_django
    mkdir -p $HOME/my-vim-django/.git
    mkdir -p $HOME/my-vim-django/pearl-metadata
    mkdir -p $HOME/my-vim-rails/.git
    mkdir -p $HOME/my-vim-rails/pearl-metadata
    create_pearl_conf "vim-rails" "$HOME/my-vim-rails" \
                      "vim-django" "$HOME/my-vim-django"
    local install_content="$(_create_install "vim-rails" post_install \
                               pre_update post_update \
                               pre_remove post_remove)"
    echo "$install_content" > $HOME/my-vim-rails/pearl-metadata/install.sh
    local install_content="$(_create_install "vim-django" post_install \
                               pre_update post_update \
                               pre_remove post_remove)"
    echo "$install_content" > $HOME/my-vim-django/pearl-metadata/install.sh
    git_mock(){
        :
    }
    GIT=git_mock
}

function create_pearl_conf(){
    local pearl_conf_content=""
    for opt in "$@"
    do
        [ -z "$1" ] && break
        local pkgname=$1
        local url=$2
        shift 2
        local content=$(cat <<EOF
PEARL_PACKAGES[$pkgname]="$url"
EOF
)
        pearl_conf_content=$(echo -e "$pearl_conf_content\n$content")
    done
    echo "$pearl_conf_content" > $PEARL_HOME/pearl.conf
}

function _create_install(){
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
        [ -z "\$PEARL_PKGVARDIR" ] && return 1
    fi
    echo "$hookfunc"
}
EOF
)
        install_content=$(echo -e "$install_content\n$content")
    done
    echo "$install_content"
}

function create_install(){
    local pkgname=$1
    local install_content="$(_create_install $@)"
    echo "$install_content" > $PEARL_HOME/packages/default/${pkgname}/pearl-metadata/install.sh
}

function create_bad_install(){
    local pkgname=$1
    local hookfunc=$2
    local install_content=$(cat <<EOF
function $hookfunc(){
    echo "$hookfunc"
    unknown_command
    return 0
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
        create_package $pkgname
        create_install "$pkgname" post_install
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 0 $?
}

function test_pearl_package_install_errors_on_hooks(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        create_package $pkgname
        create_bad_install $pkgname post_install
    }
    GIT=git_mock

    assertCommandFailOnStatus $HOOK_EXCEPTION load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 0 $?
}

function test_pearl_package_install_deinit(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        create_package $pkgname
        create_install "$pkgname" post_install
    }
    GIT=git_mock
    # Check if unset of hooks works
    load_repo_first pearl_package_install "$pkgname" > /dev/null
    type -t post_install
    assertEquals 1 $?
    [ -z "$PEARL_PKGDIR" ]
    assertEquals 0 $?
    [ -z "$PEARL_PKGVARDIR" ]
    assertEquals 0 $?
}

function test_pearl_package_install_already_installed(){
    local pkgname="ls-colors"
    scenario_misc_mods
    assertCommandFailOnStatus $ALREADY_INSTALLED_EXCEPTION load_repo_first pearl_package_install "$pkgname"
}

function test_pearl_package_install_no_install_file(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        create_package $pkgname
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 1 $?
}

function test_pearl_package_install_empty_install(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    git_mock() {
        create_package $pkgname
        echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    assertEquals "" "$(cat "$STDOUTF" | grep -v "Installing")"
}

function test_pearl_package_install_not_existing_package(){
    local pkgname="blahblah"
    scenario_misc_mods
    assertCommandFailOnStatus $NOT_IN_REPOSITORY_EXCEPTION load_repo_first pearl_package_install "$pkgname"
}

function test_pearl_local_package_install(){
    local pkgname="vim-rails"
    scenario_local_pkgs

    assertCommandSuccess load_repo_first pearl_package_install "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_install"
    assertEquals 0 $?
}

function test_pearl_local_package_install_not_existing_directory(){
    local pkgname="vim-rails"
    scenario_local_pkgs
    create_pearl_conf "$pkgname" "$HOME/my-vim-rails/no-exist"

    assertCommandFailOnStatus $LOCAL_COPY_EXCEPTION load_repo_first pearl_package_install "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/ ]
    assertEquals 0 $?
    [ ! -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDERRF | grep -q "not a directory"
    assertEquals 0 $?
}

function test_pearl_local_package_install_not_readable(){
    local pkgname="vim-rails"
    scenario_local_pkgs
    chmod -r "$HOME/my-vim-rails"
    create_pearl_conf "$pkgname" "$HOME/my-vim-rails"

    assertCommandFailOnStatus $LOCAL_COPY_EXCEPTION load_repo_first pearl_package_install "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/ ]
    assertEquals 0 $?
    [ ! -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDERRF | grep -q "not readable"
    assertEquals 0 $?
    chmod +r "$HOME/my-vim-rails"
}

function test_pearl_package_remove(){
    local pkgname="ls-colors"
    scenario_misc_mods
    assertCommandSuccess load_repo_first pearl_package_remove "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
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
    assertCommandFailOnStatus $HOOK_EXCEPTION load_repo_first pearl_package_remove "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "pre_remove"
    assertEquals 0 $?

    create_bad_install $pkgname post_remove
    assertCommandFailOnStatus $HOOK_EXCEPTION load_repo_first pearl_package_remove "$pkgname"
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
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
    [ -z "$PEARL_PKGDIR" ]
    assertEquals 0 $?
    [ -z "$PEARL_PKGVARDIR" ]
    assertEquals 0 $?
}

function test_pearl_package_remove_not_installed(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    assertCommandFailOnStatus $NOT_INSTALLED_EXCEPTION load_repo_first pearl_package_remove $pkgname
}

function test_pearl_package_remove_empty_install(){
    local pkgname="ls-colors"
    scenario_misc_mods
    echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    assertCommandSuccess load_repo_first pearl_package_remove $pkgname
    [ ! -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
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
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
}

function test_pearl_package_update(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors"; return; }
        [ "$1" == "submodule" ] && { echo "git submodule"; return; }
        echo "git pull"
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "pre_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git pull"
    assertEquals 0 $?
}

function test_pearl_package_update_url_changed(){
    local pkgname="ls-colors"
    scenario_misc_mods
    ask() {
        return 0
    }
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors2"; return; }
        if [ "$1" == "clone" ]
        then
            create_package ls-colors
            create_install "ls-colors" post_install \
                                       pre_update post_update \
                                       pre_remove post_remove
            echo "git clone"
            return
        fi
        [ "$1" == "submodule" ] && { echo "git submodule"; return; }
        echo "git pull"
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "pre_remove"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_remove"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_install"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -qv "pre_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -qv "post_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -qv "git pull"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git clone"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git submodule"
    assertEquals 0 $?
}

function test_pearl_package_update_git_config_error(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo ""; return; }
        [ "$1" == "submodule" ] && { echo "git submodule"; return; }
        echo "git pull"
    }
    GIT=git_mock

    assertCommandSuccess load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "pre_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "post_update"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git pull"
    assertEquals 0 $?
    cat "$STDOUTF" | grep -q "git submodule"
    assertEquals 0 $?
}

function test_pearl_package_update_errors_on_hooks(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors"; return; }
        echo "git pull"
    }
    GIT=git_mock

    create_bad_install $pkgname pre_update
    assertCommandFailOnStatus $HOOK_EXCEPTION load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "pre_update"
    assertEquals 0 $?

    create_bad_install $pkgname post_update
    assertCommandFailOnStatus $HOOK_EXCEPTION load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -q "post_update"
    assertEquals 0 $?
}

function test_pearl_package_update_deinit(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors"; return; }
        echo "git pull"
    }
    GIT=git_mock
    # Check if unset of hooks works
    load_repo_first pearl_package_update "$pkgname" > /dev/null
    type -t pre_update
    assertEquals 1 $?
    type -t post_update
    assertEquals 1 $?
    [ -z "$PEARL_PKGDIR" ]
    assertEquals 0 $?
    [ -z "$PEARL_PKGVARDIR" ]
    assertEquals 0 $?
}

function test_pearl_package_update_not_installed(){
    local pkgname="pearl-utils"
    scenario_misc_mods
    assertCommandFailOnStatus $NOT_INSTALLED_EXCEPTION load_repo_first pearl_package_update $pkgname
}

function test_pearl_package_update_empty_install(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors"; return; }
        return 0
    }
    GIT=git_mock
    echo "" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
    assertCommandSuccess load_repo_first pearl_package_update $pkgname
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    assertEquals "" "$(cat "$STDOUTF" | grep -v "Updating")"
}

function test_pearl_package_update_post_func_changed(){
    local pkgname="ls-colors"
    scenario_misc_mods
    git_mock() {
        [ "$1" == "config" ] && { echo "https://ls-colors"; return; }
        local content=$(cat <<EOF
function post_update(){
    echo "new_post_update"
}
EOF
)
        echo "$content" > $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh
        return 0
    }
    GIT=git_mock
    assertCommandSuccess load_repo_first pearl_package_update $pkgname
    cat "$STDOUTF" | grep -q "new_post_update"
    assertEquals 0 $?
}

function test_pearl_package_update_not_existing_package(){
    local pkgname="vim-rails"
    scenario_misc_mods
    assertCommandFailOnStatus $NOT_IN_REPOSITORY_EXCEPTION load_repo_first pearl_package_update "$pkgname"
}

function test_pearl_local_package_update(){
    local pkgname="vim-django"
    scenario_local_pkgs

    assertCommandSuccess load_repo_first pearl_package_update "$pkgname"
    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -f $PEARL_HOME/packages/default/$pkgname/pearl-metadata/install.sh ]
    assertEquals 0 $?
    [ ! -e $PEARL_HOME/packages/default/$pkgname/file_django ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDOUTF | grep -qv "post_update"
    assertEquals 0 $?
    cat $STDOUTF | grep -qv "pre_update"
    assertEquals 0 $?
}

function test_pearl_local_update_install_not_existing_directory(){
    local pkgname="vim-django"
    scenario_local_pkgs
    create_pearl_conf "$pkgname" "$HOME/my-vim-rails/no-exist"

    assertCommandFailOnStatus $LOCAL_COPY_EXCEPTION load_repo_first pearl_package_update "$pkgname"

    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -e $PEARL_HOME/packages/default/$pkgname/file_django ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDERRF | grep -q "not a directory"
    assertEquals 0 $?
}

function test_pearl_local_update_install_not_readable(){
    local pkgname="vim-django"
    scenario_local_pkgs
    chmod -r "$HOME/my-vim-rails"
    create_pearl_conf "$pkgname" "$HOME/my-vim-rails"

    assertCommandFailOnStatus $LOCAL_COPY_EXCEPTION load_repo_first pearl_package_update "$pkgname"

    [ -d $PEARL_HOME/packages/default/$pkgname/.git ]
    assertEquals 0 $?
    [ -e $PEARL_HOME/packages/default/$pkgname/file_django ]
    assertEquals 0 $?
    [ -d $PEARL_HOME/var/default/$pkgname/ ]
    assertEquals 0 $?
    cat $STDERRF | grep -q "not readable"
    assertEquals 0 $?
    chmod +r "$HOME/my-vim-rails"
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
        "$(echo -e "pull --quiet\nsourced repo.conf")" "$(cat $STDOUTF | grep -v "Updating")"
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

function test_package_full_name_from_local() {
    scenario_misc_mods
    _package_full_name_from_local ls-colors
    assertEquals 0 $?
    assertEquals "default/ls-colors" "$RESULT"
    unset RESULT
}

function test_package_full_name_from_local_with_fullname() {
    scenario_misc_mods
    _package_full_name_from_local default/ls-colors
    assertEquals 0 $?
    assertEquals "default/ls-colors" "$RESULT"
    unset RESULT
}

function test_package_full_name_from_local_null_arg() {
    scenario_misc_mods
    assertCommandFailOnStatus 11 _package_full_name_from_local ""
}

function test_package_full_name_from_local_no_pkg() {
    scenario_misc_mods
    _package_full_name_from_local ls-colors2
    assertEquals 0 $?
    assertEquals "" "$RESULT"
    unset RESULT
}

function test_package_full_name() {
    scenario_misc_mods
    _package_full_name ls-colors
    assertEquals 0 $?
    assertEquals "default/ls-colors" "$RESULT"
    unset RESULT
}

function test_package_full_name_with_fullname() {
    scenario_misc_mods
    _package_full_name default/ls-colors
    assertEquals 0 $?
    assertEquals "default/ls-colors" "$RESULT"
    unset RESULT
}

function test_package_full_name_null_arg() {
    scenario_misc_mods
    assertCommandFailOnStatus 11 _package_full_name ""
}

function test_package_full_name_no_pkg() {
    scenario_misc_mods
    _package_full_name ls-colors2
    assertEquals 0 $?
    assertEquals "" "$RESULT"
    unset RESULT
}

function test_get_list_installed_packages() {
    scenario_misc_mods
    get_list_installed_packages ".*"
    assertEquals 0 $?
    local expected="$(echo -e "default/ls-colors default/pearl-ssh default/vim-rails")"
    assertEquals "$expected" "$(echo "${RESULT[@]}")"
    unset RESULT
}

function test_get_list_installed_packages_with_pattern() {
    scenario_misc_mods
    get_list_installed_packages "pearl"
    assertEquals 0 $?
    assertEquals "default/pearl-ssh" "${RESULT[@]}"
    unset RESULT
}

function test_get_list_installed_packages_no_pattern() {
    scenario_misc_mods
    get_list_installed_packages ""
    assertEquals 0 $?
    local expected="$(echo -e "default/ls-colors default/pearl-ssh default/vim-rails")"
    assertEquals "$expected" "$(echo "${RESULT[@]}")"
    unset RESULT
}

function test_get_list_installed_packages_no_match() {
    scenario_misc_mods
    get_list_installed_packages "no-match"
    assertEquals 0 $?
    assertEquals "x" "x${RESULT[@]}"
    unset RESULT
}

function test_get_list_installed_packages_empty() {
    get_list_installed_packages ".*"
    assertEquals 0 $?
    assertEquals "x" "x${RESULT[@]}"
    unset RESULT
}

function test_get_list_uninstalled_packages() {
    scenario_misc_mods
    get_list_uninstalled_packages ".*"
    assertEquals 0 $?
    assertEquals "default/pearl-utils" "${RESULT[@]}"
    unset RESULT
}

function test_get_list_uninstalled_packages_with_pattern() {
    scenario_misc_mods
    get_list_uninstalled_packages "pearl"
    assertEquals 0 $?
    assertEquals "default/pearl-utils" "${RESULT[@]}"
    unset RESULT
}

function test_get_list_uninstalled_packages_no_pattern() {
    scenario_misc_mods
    get_list_uninstalled_packages ""
    assertEquals 0 $?
    assertEquals "default/pearl-utils" "${RESULT[@]}"
    unset RESULT
}

function test_get_list_uninstalled_packages_no_match() {
    scenario_misc_mods
    get_list_uninstalled_packages "no-match"
    assertEquals 0 $?
    assertEquals "x" "x${RESULT[@]}"
    unset RESULT
}

source $(dirname $0)/shunit2
