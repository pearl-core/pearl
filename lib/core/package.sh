# This module contains all functionalities needed for
# handling the pearl packages.
#
# Dependencies:
# - lib/utils.sh
#
# vim: ft=sh

set -e

declare -a PEARL_INTERNAL_REPOS_NAME
declare -A PEARL_INTERNAL_PACKAGES
declare -A PEARL_INTERNAL_PACKAGES_DESCR

GIT=git
OLD_PWD=${PWD}

# Load the information coming from the pearl.conf and the repositories.
# This function build namespaces based on the repository name. This will avoid
# potential clashes between packages belonging to different repositories.
#
# The outcome of this function is a set of PEARL_INTERNAL* variables that will
# be used by the pearl script.
#
# This function will erase PEARL_PACKAGES and PEARL_REPOS as soon as PEARL_INTERNAL* are built.
#
function pearl_load_repos() {
    declare -a PEARL_REPOS
    _load_internal_repo "$PEARL_HOME/pearl.conf"
    for repo in "${PEARL_REPOS[@]}"
    do
        _load_repo "$repo"
    done
    unset PEARL_REPOS
}

function _load_internal_repo() {
    local confname=$1
    declare -A PEARL_PACKAGES
    declare -A PEARL_PACKAGES_DESCR
    source "$confname"
    [ -z "$PEARL_REPO_NAME" ] && PEARL_REPO_NAME="default"
    PEARL_INTERNAL_REPOS_NAME+=($PEARL_REPO_NAME)
    for pkgname in "${!PEARL_PACKAGES[@]}"
    do
        PEARL_INTERNAL_PACKAGES[$PEARL_REPO_NAME/$pkgname]=${PEARL_PACKAGES[$pkgname]}
        PEARL_INTERNAL_PACKAGES_DESCR[$PEARL_REPO_NAME/$pkgname]=${PEARL_PACKAGES_DESCR[$pkgname]}
    done
    unset PEARL_PACKAGES PEARL_PACKAGES_DESCR PEARL_REPO_NAME
}
function _load_repo() {
    local repo=$1
    local sum=$(echo "$repo" | md5sum | cut -d ' ' -f1)
    if [ -d "$PEARL_HOME/repos/$sum/.git" ]; then
        bold_white; echo -n "* "; normal
        echo "Updating $repo repository"
        cd "$PEARL_HOME/repos/$sum"
        $GIT pull --quiet origin master
        cd "$OLD_PWD"
    else
        bold_white; echo -n "* "; normal
        echo "Initializing $repo repository"
        $GIT clone --quiet --depth 1 "$repo" "$PEARL_HOME/repos/$sum"
    fi
    _load_internal_repo "$PEARL_HOME/repos/$sum/repo.conf"
}

function _package_full_name() {
    local pkgname=$1
    if [[ $pkgname == *[/]* ]]
    then
        echo $pkgname
        return
    fi

    for reponame in "${PEARL_INTERNAL_REPOS_NAME[@]}"
    do
        [ ${PEARL_INTERNAL_PACKAGES[$reponame/$pkgname]+abc} ] && { echo "$reponame/$pkgname" ; return; }
    done
    echo ""
}

function pearl_package_install(){
    local pkgname=$1
    local post_func=post_install

    local pkgfullname=$(_package_full_name $pkgname)
    [ -z "$pkgfullname" ] && { warn "Skipping $pkgname is not in the repositories."; return 2; }
    _is_installed $pkgfullname && { warn "Skipping $pkgname since it is already installed."; return 1; }

    bold_white; echo -n "* "; normal
    echo "Installing $pkgfullname package"
    mkdir -p $PEARL_HOME/packages/$pkgfullname
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    cd $PEARL_PKGDIR
    $GIT clone --quiet --depth 1 "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" "${PEARL_PKGDIR}"
    $GIT submodule --quiet update --depth 1 --init --remote
    _init_package "$pkgfullname" "" $post_func
    if type -t $post_func &> /dev/null
    then
        $post_func || { error "Error on executing '$post_func' hook."; return 4; }
    fi

    unset PEARL_PKGDIR
    _deinit_package $pkgfullname $pre_func $post_func
}

function pearl_package_update(){
    local pkgname=$1
    local pre_func=pre_update
    local post_func=post_update

    local pkgfullname=$(_package_full_name $pkgname)
    [ -z "$pkgfullname" ] && { warn "Skipping $pkgname is not in the repositories."; return 2; }
    ! _is_installed $pkgfullname && { warn "Skipping $pkgname since it has not been installed."; return 1; }

    _init_package $pkgfullname $pre_func $post_func

    bold_white; echo -n "* "; normal
    echo "Updating $pkgfullname package"
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    cd $PEARL_PKGDIR
    if type -t $pre_func &> /dev/null
    then
        $pre_func || { error "Error on executing '$pre_func' hook."; return 3; }
    fi
    $GIT pull --quiet origin master
    $GIT submodule --quiet update --depth 1 --init --remote
    if type -t $post_func &> /dev/null
    then
        $post_func || { error "Error on executing '$post_func' hook."; return 4; }
    fi

    unset PEARL_PKGDIR
    _deinit_package $pkgfullname $pre_func $post_func
}

function pearl_package_remove(){
    local pkgname=$1
    local pre_func=pre_remove
    local post_func=post_remove

    local pkgfullname=$(_package_full_name $pkgname)
    ! _is_installed $pkgfullname && { warn "Skipping $pkgname since it has not been installed."; return 1; }

    _init_package $pkgfullname $pre_func $post_func

    bold_white; echo -n "* "; normal
    echo "Removing $pkgfullname package"
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    cd $PEARL_PKGDIR
    if type -t $pre_func &> /dev/null
    then
        $pre_func || { error "Error on executing '$pre_func' hook."; return 3; }
    fi
    cd $PEARL_HOME
    rm -rf $PEARL_HOME/packages/$pkgfullname
    if type -t $post_func &> /dev/null
    then
        $post_func || { error "Error on executing '$post_func' hook."; return 4; }
    fi

    unset PEARL_PKGDIR
    _deinit_package $pkgfullname $pre_func $post_func
}

function _is_installed() {
    local pkgfullname=$1
    [ -d "$PEARL_HOME/packages/$pkgfullname/.git" ]
}

function _init_package(){
    local pkgfullname=$1
    local pre_func=$2
    local post_func=$3

    unset ${pre_func} ${post_func}
    local hook_file=${PEARL_HOME}/packages/$pkgfullname/pearl-metadata/install.sh
    [ -f "$hook_file" ] && source "$hook_file"
    return 0
}

function _deinit_package(){
    local pkgfullname=$1
    local pre_func=$2
    local post_func=$3
    unset ${pre_func} ${post_func}
}

function pearl_package_list(){
    local pattern=".*"
    [ -z "$1" ] || pattern="$1"
    builtin cd $PEARL_ROOT
    for pkg in $(get_list_removed_packages $pattern)
    do
        _print_package $pkg false
    done
    for pkg in $(get_list_installed_packages $pattern)
    do
        _print_package $pkg true
    done
    builtin cd $OLDPWD
}

function get_list_installed_packages(){
    local pattern=$1
    for pkgfullname in "${!PEARL_INTERNAL_PACKAGES[@]}"
    do
        [ -e $PEARL_HOME/packages/$pkgfullname/.git ] && { echo $pkgfullname | grep "$pattern"; }
    done
}

function get_list_removed_packages(){
    local pattern=$1
    for pkgfullname in "${!PEARL_INTERNAL_PACKAGES[@]}"
    do
        [ ! -e $PEARL_HOME/packages/$pkgfullname/.git ] && { echo $pkgfullname | grep "$pattern"; }
    done
}

function _print_package() {
    local pkgfullname=$1
    local installed=""
    $2 && installed="[installed]"
    local pkg_array=(${pkgfullname//\// })
    local reponame=${pkg_array[0]}
    local pkgname=${pkg_array[1]}
    bold_red
    echo -n "$reponame/"
    bold_white
    echo -n "$pkgname"
    bold_cyan
    echo " $installed"
    normal
    echo "    ${PEARL_INTERNAL_PACKAGES_DESCR[$pkgfullname]}"
}
