# This module contains utility functions for ensuring the compatibility with
# OSX systems.
#
# Dependencies:
#   None
#
# vim: ft=sh

GNUBIN="/usr/local/opt/coreutils/libexec/gnubin"

#######################################
# Update PATH variable environment with the GNUBIN directory.
# This function is useful for OSX systems in order to ensure that
# GNU executables have major priority against the local executables.
#
# Globals:
#   PATH   (WO)     : Put the GNUBIN directory in top of the PATH variable.
#   GNUBIN (RO)     : The GNUBIN directory.
# Arguments:
#   None
# Returns:
#   None
# Output:
#   None
#######################################
function pearl_update_path() {
    [ -d "$GNUBIN" ] && PATH="$GNUBIN:$PATH"
    return 0
}


#######################################
# Attempt to execute the given command first using the one located in GNUBIN
# directory. If the executable does not exist in GNUBIN, the function attempts
# to execute the command located in the usual paths defined by PATH variable.
#
# This function is useful for OSX systems in order to ensure that
# GNU executables have major priority against the local executables.
#
# The difference with pearl_update_path is that the current function does
# not pollute PATH variable.
#
# Globals:
#   GNUBIN (RO)  : The GNUBIN directory in which to detect the executable.
# Arguments:
#   cmd  ($1)    : The command to execute.
#   args ($2-)   : The command arguments.
# Returns:
#   -            : The command return.
# Output:
#   -            : The command output.
#######################################
function pearl_attempt_command() {
    local cmd=$1
    shift
    if [ -x "$GNUBIN/$cmd" ]
    then
        "$GNUBIN/$cmd" $@
    else
        $cmd $@
    fi
}
