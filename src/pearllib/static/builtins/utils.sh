#!/bin/sh
# This module contains all functionalities needed for
# handling the pearl packages.
#
# Dependencies:
# - [buava] $PEARL_ROOT/buava/lib/utils.sh
#
# vim: ft=sh

#######################################
# Link executable file to PATH variable by creating
# a symlink to the default $PEARL_HOME/bin directory.
#
# The function is idempotent, so calling this function multiple
# times will link the executable file once.
#
# If $executable_path does not exist, the function will fail.
#
# Example of usage:
#    link_to_path "~/myexecfile"
#
# Globals:
#   PEARL_HOME (RO)       : Used to locate $PEARL_HOME/bin.
# Arguments:
#   executable_path ($1)  : The executable file path.
#   executable_name ($2?) : The new executable name.
# Returns:
#   NO_FILE_OR_DIRECTORY  : $executable_path does not exist.
#   0                     : Successfully linked.
# Output:
#   None
#######################################
function link_to_path() {
    local executable_path=$1
    check_not_null ${executable_path}

    local default_executable_name=$(basename "$executable_path")
    local executable_name=${2:-${default_executable_name}}
    link_to "${executable_path}" "${PEARL_HOME}/bin/${executable_name}"
}

#######################################
# Unlink the executable file to PATH variable
# by removing the symlink from the default
# $PEARL_HOME/bin directory.
#
# The function is idempotent, so calling this function multiple
# times will unlink the file once.
#
# If the symlink is broken, the symlink will be deleted.
# If the symlink corresponds to a different source file path
# from $executable_path, the symlink will not be deleted but an
# error will be raised.
#
# Example of usage:
#    unlink_from_path "~/myexecfile"
#
# Globals:
#   PEARL_HOME (RO)      : Used to locate $PEARL_HOME/bin.
# Arguments:
#   executable_path ($1) : The executable file path.
#   executable_name ($2?) : The new executable name.
# Returns:
#   0                    : Successfully unlinked.
#   36                   : Symlink exists on a differt source file.
# Output:
#   None
#######################################
function unlink_from_path() {
    local executable_path=$1
    check_not_null ${executable_path}

    local default_executable_name=$(basename "$executable_path")
    local executable_name=${2:-${default_executable_name}}

    unlink_from "${executable_path}" "${PEARL_HOME}/bin/${executable_name}"
}
