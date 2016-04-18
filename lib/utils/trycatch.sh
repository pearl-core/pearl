# This module is an improvement of the suggested implementation of try/catch in
# Bash here: http://stackoverflow.com/a/25180186
#
# This module allows to invoke a certain command and capture its status in case of failures.
#
# If `set -e` is set, the program
# will stop if the command fails, instead `try` will make sure `set -e` is
# still preserved inside the command and the status of command will be returned.
#
# The following is an example that will not work:
#
#    set -e
#    my_func || echo "my_func returned non-zero status"
#
# In the previous example the `set -e` is completely erased inside the invocation of
# `my_func`. In other words, any unexpected error that occurs in `my_func` will not stop the
# execution of the function.
#
# `try` tries to overcome this issue by making sure that `my_func` fails as
# soon as an error raises:
#
#    try my_func
#    catch || {
#        case $? in
#        $AN_EXCEPTION)
#            echo "AN_EXCEPTION was thrown"
#        ;;
#        $ANOTHER_EXCEPTION)
#            echo "ANOTHER_EXCEPTION was thrown"
#        ;;
#        *)
#            echo "An unexpected exception was thrown"
#            throw 333 # you can rethrow the "exception" causing the script to exit if not caught
#        ;;
#        esac
#    }
#
# Dependencies:
# - lib/utils/utils.sh
#
# vim: ft=sh

#######################################
# Invoke a certain command in a subshell and capture its status in case of failures.
#
# Globals:
#   SAVED_OPT_E (WO) : Keep the error option before the execution of the `try`.
#   INSIDE_TRY (WO)  : Identify whether a code block is inside a try/catch.
# Arguments:
#   command ($@)     : The command to be executed.
# Returns:
#   1                : Any unexpected error from the command.
#   $?               : Any error explicitly invoked inside the command
#                      via `return` or `exit`.
# Output:
#   The output of the command.
#######################################
function try() {
    check_not_null $1
    [[ $- = *e* ]]; SAVED_OPT_E=$?
    INSIDE_TRY=true
    set +e
    (
      set -e
      "$@"
    )
}

#######################################
# Change the error option according to SAVED_OPT_E variable and disable the
# INSIDE_TRUE.
#
# Globals:
#   SAVED_OPT_E (RO): To check whether error option has to be enabled or not.
# Arguments:
#   The status code from the try block.
# Returns:
#   The status code from the try block
#######################################
function catch()
{
    local ex_code=$?
    [[ $SAVED_OPT_E -eq 0 ]] && set -e || set +e
    unset INSIDE_TRY
    return $ex_code
}


#######################################
# Used inside a try/catch block, the function will `exit` with the given
# status code.
# The function can be even used outside a try/catch block and,
# in that case, it will use `return` command instead of `exit`.
#
# Globals:
#   INSIDE_TRY (RO) : To check if `throw` has been invoked inside a try/catch block.
# Arguments:
#   exception ($1)  : The exception to throw
# Returns:
#   The exception
#######################################
function throw()
{
    check_not_null $1
    [ -z "$INSIDE_TRY" ] || exit $1
    return $1
}

