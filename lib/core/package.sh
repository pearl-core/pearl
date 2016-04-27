# This module contains all functionalities needed for
# handling the pearl packages.
#
# Dependencies:
# - lib/utils/utils.sh
#
# vim: ft=sh

set -e

declare -a PEARL_INTERNAL_REPOS_NAME
declare -A PEARL_INTERNAL_PACKAGES
declare -A PEARL_INTERNAL_PACKAGES_DESCR

RM=rm
CP=cp
GIT=git
OLD_PWD=${PWD}

HOOK_EXCEPTION=101
ALREADY_INSTALLED_EXCEPTION=102
NOT_INSTALLED_EXCEPTION=103
NOT_IN_REPOSITORY_EXCEPTION=104
LOCAL_COPY_EXCEPTION=105

#######################################
# Load the information coming from the pearl.conf and the repositories.
# This function build namespaces based on the repository name. This will avoid
# potential clashes between packages belonging to different repositories.
#
# The outcome of this function is a set of PEARL_INTERNAL* variables that will
# be used by the pearl script.
#
# This function will erase PEARL_PACKAGES and PEARL_REPOS as soon as PEARL_INTERNAL* are built.
#
# Globals:
#   PEARL_REPOS (RO)                   : Array containing the list
#                                        of repos defined in pearl.conf.
#   PEARL_PACKAGES (RO)                : Array containing the list
#                                        of packages defined in pearl.conf.
#   PEARL_INTERNAL_REPOS_NAME (WO)     : Array containing the list
#                                        of repo names
#   PEARL_INTERNAL_PACKAGES (WO)       : Array containing the list of packages.
#   PEARL_INTERNAL_PACKAGES_DESCR (WO) : Array containing the list
#                                        of package descriptions.
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Logging information.
#######################################
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

#######################################
# Provide the full name of a package by reading the repository.
#
# Globals:
#   RESULT (WO)     : Contains the package full name.
# Arguments:
#   pkgname ($1)    : The name of the package.
# Returns:
#   None
# Output:
#   None
#######################################
function _package_full_name() {
    local pkgname=$1
    check_not_null $pkgname

    RESULT=""
    if [[ $pkgname == *[/]* ]]
    then
        RESULT="$pkgname"
        return
    fi

    for reponame in "${PEARL_INTERNAL_REPOS_NAME[@]}"
    do
        [ ${PEARL_INTERNAL_PACKAGES[$reponame/$pkgname]+abc} ] && { RESULT="$reponame/$pkgname" ; return; }
    done
    return 0
}

#######################################
# Provide the full name of a package by reading the local directory.
#
# Globals:
#   PEARL_HOME (RO) : Used to access to the local directory.
#   RESULT (WO)     : Contains the package full name.
# Arguments:
#   pkgname ($1)    : The name of the package.
# Returns:
#   None
# Output:
#   None
#######################################
function _package_full_name_from_local() {
    local pkgname=$1
    check_not_null $pkgname

    RESULT=""
    cd $PEARL_HOME/packages
    if [[ $pkgname == *[/]* ]]
    then
        if [ -d "$pkgname" ]
        then
            RESULT="$pkgname"
        fi
        return
    fi

    for reponame in $(ls ${PEARL_HOME}/packages/)
    do
        [ -d "$reponame/$pkgname" ] && { RESULT="$reponame/$pkgname"; return; }
    done
    return 0
}

function _is_local_package(){
    local pkgurl=$1
    [[ "$pkgurl" == /* ]]
}

function _check_and_remove(){
    local destdir=$1
    [ -e "${destdir}" ] && $RM -rf "${destdir}"
    return 0
}

function _check_and_copy(){
    local sourcedir=$1
    local destdir=$2
    [ -d "${sourcedir}" ] || { error "Error: $sourcedir is not a directory"; return 1; }
    [ -r "${sourcedir}" ] || { error "Error: $sourcedir is not readable"; return 2; }
    _check_and_remove "${destdir}"
    $CP -r "${sourcedir}" "${destdir}"
}

#######################################
# Install the Pearl package.
#
# Globals:
#   PEARL_HOME (RO)                 : Used to access to the local directory.
#   PEARL_INTERNAL_PACKAGES (RO)    : Used to get the package location.
# Arguments:
#   pkgname ($1)                    : The name of the package.
# Returns:
#   NOT_IN_REPOSITORY_EXCEPTION     : The package is not available in repo.
#   ALREADY_INSTALLED_EXCEPTION     : The package has already been installed.
#   LOCAL_COPY_EXCEPTION            : Error during the local copy.
#   HOOK_EXCEPTION                  : Error during the hook function execution.
# Output:
#   Logging information.
#######################################
function pearl_package_install(){
    local pkgname=$1
    local post_func=post_install

    _package_full_name $pkgname
    local pkgfullname=$RESULT
    unset RESULT
    [ -z "$pkgfullname" ] && { warn "Skipping $pkgname is not in the repositories."; throw $NOT_IN_REPOSITORY_EXCEPTION; }
    _is_installed $pkgfullname && { warn "Skipping $pkgname since it is already installed."; throw $ALREADY_INSTALLED_EXCEPTION; }

    bold_white; echo -n "* "; normal
    echo "Installing $pkgfullname package"
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    mkdir -p $(dirname "$PEARL_PKGDIR")
    if _is_local_package "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}"
    then
        _check_and_copy "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" "${PEARL_PKGDIR}" || { _deinit_package $pkgfullname $pre_func $post_func; throw $LOCAL_COPY_EXCEPTION; }
    else
        $GIT clone --quiet "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" "${PEARL_PKGDIR}"
        cd "$PEARL_PKGDIR"
        $GIT submodule --quiet update --init
    fi
    cd "$PEARL_PKGDIR"
    _init_package "$pkgfullname" "" $post_func
    PEARL_PKGVARDIR=$PEARL_HOME/var/$pkgfullname
    mkdir -p "$PEARL_PKGVARDIR"
    if type -t $post_func &> /dev/null
    then
        try $post_func
        catch || {
            error "Error on executing '$post_func' hook.";
            _deinit_package $pkgfullname $pre_func $post_func;
            throw $HOOK_EXCEPTION;
        }
    fi

    _deinit_package $pkgfullname $pre_func $post_func
}

function _is_url_changed(){
    local pkgfullname=$1
    local existingurl=$($GIT config remote.origin.url)
    # Compare the Git URL only if git config produce an non empty string
    [ -z "$existingurl" ] && return 1
    [ "$existingurl" != "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" ]
}

#######################################
# Update the Pearl package.
# If the Pearl package location changed (i.e. repo has been updated),
# the function will ask to replace the existing package.
#
# Globals:
#   PEARL_HOME (RO)                 : Used to access to the local directory.
#   PEARL_INTERNAL_PACKAGES (RO)    : Used to get the package location.
# Arguments:
#   pkgname ($1)                    : The name of the package.
# Returns:
#   NOT_IN_REPOSITORY_EXCEPTION     : The package is not available in repo.
#   NOT_INSTALLED_EXCEPTION         : The package has not been installed.
#   LOCAL_COPY_EXCEPTION            : Error during the local copy.
#   HOOK_EXCEPTION                  : Error during the hook function execution.
# Output:
#   Logging information.
#######################################
function pearl_package_update(){
    local pkgname=$1
    local pre_func=pre_update
    local post_func=post_update

    _package_full_name $pkgname
    local pkgfullname=$RESULT
    unset RESULT
    [ -z "$pkgfullname" ] && { warn "Skipping $pkgname is not in the repositories."; throw $NOT_IN_REPOSITORY_EXCEPTION; }
    ! _is_installed $pkgfullname && { warn "Skipping $pkgname since it has not been installed."; throw $NOT_INSTALLED_EXCEPTION; }

    _init_package $pkgfullname $pre_func $post_func

    bold_white; echo -n "* "; normal
    echo "Updating $pkgfullname package"
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    PEARL_PKGVARDIR=$PEARL_HOME/var/$pkgfullname
    cd $PEARL_PKGDIR

    if ! _is_local_package "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" && \
        _is_url_changed $pkgfullname
    then
        echo "The Git URL for $pkgfullname has changed to ${PEARL_INTERNAL_PACKAGES[$pkgfullname]}"
        if ask "Do you want to replace the package with the new repository?" "N"
        then
            pearl_package_remove $pkgfullname
            pearl_package_install $pkgfullname
        fi
        return 0
    fi

    if type -t $pre_func &> /dev/null
    then
        try $pre_func
        catch || {
            error "Error on executing '$pre_func' hook.";
            _deinit_package $pkgfullname $pre_func $post_func;
            throw $HOOK_EXCEPTION;
        }
    fi
    if _is_local_package "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}"
    then
        _check_and_copy "${PEARL_INTERNAL_PACKAGES[$pkgfullname]}" "${PEARL_PKGDIR}" || { _deinit_package $pkgfullname $pre_func $post_func; throw $LOCAL_COPY_EXCEPTION; }
    else
        $GIT pull --quiet
        $GIT submodule --quiet update --init
    fi
    if type -t $post_func &> /dev/null
    then
        try $post_func
        catch || {
            error "Error on executing '$post_func' hook.";
            _deinit_package $pkgfullname $pre_func $post_func;
            throw $HOOK_EXCEPTION;
        }
    fi

    _deinit_package $pkgfullname $pre_func $post_func
}

#######################################
# Remove the Pearl package.
#
# Globals:
#   PEARL_HOME (RO)                 : Used to access to the local directory.
# Arguments:
#   pkgname ($1)                    : The name of the package.
# Returns:
#   NOT_INSTALLED_EXCEPTION         : The package has not been installed.
#   HOOK_EXCEPTION                  : Error during the hook function execution.
# Output:
#   Logging information.
#######################################
function pearl_package_remove(){
    local pkgname=$1
    local pre_func=pre_remove
    local post_func=post_remove

    _package_full_name_from_local $pkgname
    local pkgfullname=$RESULT
    unset RESULT
    [ -z "$pkgfullname" ] && { warn "Skipping $pkgname since it has not been installed."; throw $NOT_INSTALLED_EXCEPTION; }

    _init_package $pkgfullname $pre_func $post_func

    bold_white; echo -n "* "; normal
    echo "Removing $pkgfullname package"
    PEARL_PKGDIR=$PEARL_HOME/packages/$pkgfullname
    PEARL_PKGVARDIR=$PEARL_HOME/var/$pkgfullname
    cd $PEARL_PKGDIR
    if type -t $pre_func &> /dev/null
    then
        try $pre_func
        catch || {
            error "Error on executing '$pre_func' hook.";
            _deinit_package $pkgfullname $pre_func $post_func;
            throw $HOOK_EXCEPTION;
        }
    fi
    cd $PEARL_HOME
    _check_and_remove "${PEARL_PKGDIR}"
    if type -t $post_func &> /dev/null
    then
        try $post_func
        catch || {
            error "Error on executing '$post_func' hook.";
            _deinit_package $pkgfullname $pre_func $post_func;
            throw $HOOK_EXCEPTION;
        }
    fi

    _deinit_package $pkgfullname $pre_func $post_func
}

function _is_installed() {
    local pkgfullname=$1
    [ -d "$PEARL_HOME/packages/$pkgfullname" ]
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
    unset PEARL_PKGDIR
    unset PEARL_PKGVARDIR
    unset ${pre_func} ${post_func}
}

#######################################
# List/Search for the Pearl packages.
#
# Globals:
#   PEARL_HOME (RO)                 : Used to access to the local directory.
# Arguments:
#   pattern ($1)                    : Pattern of the packages to search.
# Returns:
#   None
# Output:
#   The list of installed/uninstalled packages.
#######################################
function pearl_package_list(){
    local pattern=".*"
    [ -z "$1" ] || pattern="$1"
    declare -a RESULT
    get_list_uninstalled_packages "$pattern"
    for pkg in "${RESULT[@]}"
    do
        _print_package $pkg false
    done
    unset RESULT
    declare -a RESULT
    get_list_installed_packages "$pattern"
    for pkg in "${RESULT[@]}"
    do
        _print_package $pkg true
    done
    unset RESULT
}

#######################################
# Get the list of all installed packages by reading the local directory.
#
# Globals:
#   PEARL_HOME (RO)     : Used to access to the local directory.
#   RESULT (WO)         : Array containing the list of installed packages.
# Arguments:
#   pattern ($1)        : The name of the package.
# Returns:
#   None
# Output:
#   None
#######################################
function get_list_installed_packages() {
    local pattern=$1

    for reponame in $(ls ${PEARL_HOME}/packages/)
    do
        for pkgname in $(ls ${PEARL_HOME}/packages/${reponame}/)
        do
            [[ "$reponame/$pkgname" =~ .*$pattern.* ]] && RESULT+=("$reponame/$pkgname")
        done
    done
    return 0
}

#######################################
# Get the list of all uninstalled packages from the repository.
#
# Globals:
#   PEARL_HOME (RO)   : Used to access to the local directory.
#   RESULT (WO)       : Array containing the list of uninstalled packages.
# Arguments:
#   pattern ($1)      : The name of the package.
# Returns:
#   None
# Output:
#   None
#######################################
function get_list_uninstalled_packages() {
    local pattern=$1

    for pkgfullname in "${!PEARL_INTERNAL_PACKAGES[@]}"
    do
        [ ! -d $PEARL_HOME/packages/$pkgfullname/ ] && [[ "$pkgfullname" =~ .*$pattern.* ]] && RESULT+=("$pkgfullname")
    done
    return 0
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
