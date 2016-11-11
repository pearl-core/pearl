#!/bin/sh

declare -A CONFIG_FILES
CONFIG_FILES[bash]="$HOME/.bashrc"
CONFIG_FILES[emacs]="$HOME/.emacs"
CONFIG_FILES[fish]="$HOME/.config/fish/config.fish"
CONFIG_FILES[git]="$HOME/.gitconfig"
CONFIG_FILES[inputrc]="$HOME/.inputrc"
CONFIG_FILES[mutt]="$HOME/.muttrc"
CONFIG_FILES[screen]="$HOME/.screenrc"
CONFIG_FILES[tmux]="$HOME/.tmux.conf"
CONFIG_FILES[vim]="$HOME/.vimrc"
CONFIG_FILES[zsh]="$HOME/.zshrc"

declare -A SOURCE_LINES
SOURCE_LINES[bash]="source \"{}\""
SOURCE_LINES[emacs]="(load-file \"{}\")"
SOURCE_LINES[fish]="source \"{}\""
SOURCE_LINES[git]="[include] path = \"{}\""
SOURCE_LINES[inputrc]="\$include {}"
SOURCE_LINES[mutt]="source {}"
SOURCE_LINES[screen]="source {}"
SOURCE_LINES[tmux]="source {}"
SOURCE_LINES[vim]="source {}"
SOURCE_LINES[zsh]="source \"{}\""

NULL_EXCEPTION=11
WRONG_ANSWER=33
NO_FILE_OR_DIRECTORY=2

#######################################
# Check if the argument is null.
#
# Globals:
#   None
# Arguments:
#   argument ($1)    : Argument to check.
# Returns:
#   0                : If argument is not null.
#   NULL_EXCEPTION   : If argument is null.
# Output:
#   None
#######################################
function check_not_null() {
    [ -z "$1" ] && { error "Error: null argument $1"; return $NULL_EXCEPTION; }
    return 0
}

#######################################
# Redirect message to stderr.
#
# Globals:
#   None
# Arguments:
#   msg ($@): Message to print.
# Returns:
#   None
# Output:
#   Message printed to stderr.
#######################################
function echoerr() {
    echo "$@" 1>&2;
}

#######################################
# Print an error message to stderr and exit program.
#
# Globals:
#   None
# Arguments:
#   msg ($@)   : Message to print.
# Returns:
#   1          : The unique exit status printed.
# Output:
#   Message printed to stderr.
#######################################
function die() {
    error $@
    exit 1
}

#######################################
# Print an error message to stderr and exit program with a given status.
#
# Globals:
#   None
# Arguments:
#   status ($1)     : The exit status to use.
#   msg ($2-)       : Message to print.
# Returns:
#   $?              : The $status exit status.
# Output:
#   Message printed to stderr.
#######################################
function die_on_status() {
    status=$1
    shift
    error $@
    exit $status
}

#######################################
# Print an error message to stderr.
#
# Globals:
#   None
# Arguments:
#   msg ($@): Message to print.
# Returns:
#   None
# Output:
#   Message printed to stderr.
#######################################
function error() {
    echoerr -e "\033[1;31m$@\033[0m"
}

#######################################
# Print a warn message to stderr.
#
# Globals:
#   None
# Arguments:
#   msg ($@): Message to print.
# Returns:
#   None
# Output:
#   Message printed to stderr.
#######################################
function warn() {
    # $@: msg (mandatory) - str: Message to print
    echoerr -e "\033[1;33m$@\033[0m"
}

#######################################
# Print an info message to stdout.
#
# Globals:
#   None
# Arguments:
#   msg ($@): Message to print.
# Returns:
#   None
# Output:
#   Message printed to stdout.
#######################################
function info(){
    echo -e "\033[1;36m$@\033[0m"
}

#######################################
# Print escape chars to activate the bold white style.
#
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Print the bold white escape chars.
#######################################
function bold_white(){
    echo -ne "\033[1;37m"
}

#######################################
# Print escape chars to activate the bold cyan style.
#
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Print the bold white escape chars.
#######################################
function bold_cyan(){
    echo -ne "\033[1;36m"
}

#######################################
# Print escape chars to activate the bold cyan style.
#
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Print the bold cyan escape chars.
#######################################
function bold_cyan(){
    echo -ne "\033[1;36m"
}

#######################################
# Print escape chars to activate the bold red style.
#
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Print the bold red escape chars.
#######################################
function bold_red(){
    echo -ne "\033[1;35m"
}

#######################################
# Print escape char to deactivate any style.
#
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
# Output:
#   Print the normal escape chars.
#######################################
function normal(){
    echo -ne "\033[0m"
}

#######################################
# Ask a question and wait to receive an answer from stdin.
# It returns $default_answer if no answer has be received from stdin.
#
# Globals:
#   None
# Arguments:
#   question ($1)       : The question to ask.
#   default_answer ($2) : Possible values: 'Y', 'y', 'N', 'n' (default: 'Y')
# Returns:
#   0                   : If user replied with either 'Y' or 'y'.
#   1                   : If user replied with either 'N' or 'n'.
#   WRONG_ANSWER        : If `default_answer` is not one of the possible values.
# Output:
#   Print the question to ask.
#######################################
function ask(){
    local question=$1
    local default_answer=$2
    check_not_null $question

    if [ ! -z "$default_answer" ]
    then
        local answers="Y y N n"
        [[ "$answers" =~ "$default_answer" ]] || { error "The default answer: $default_answer is wrong."; return $WRONG_ANSWER; }
    fi

    local default="Y"
    [ -z "$default_answer" ] || default=$(echo "$default_answer" | tr '[:lower:]' '[:upper:]')

    local other="n"
    [ "$default" == "N" ] && other="y"

    local prompt=$(info "$question (${default}/${other})> ")

    local res="none"
    while [ "$res" != "Y" ] && [ "$res" != "N"  ] && [ "$res" != "" ];
    do
        read -p "$prompt" res
        res=$(echo "$res" | tr '[:lower:]' '[:upper:]')
    done

    [ "$res" == "" ] && res="$default"

    [ "$res" == "Y" ]
}


#######################################
# Apply a string to a file.
# The function is idempotent, so calling this function multiple
# times will apply the string once.
# If $config_file does not exist, the function will create the file and all its
# parent directories (if needed).
#
# Example of usage:
#    apply "source ~/myvimrcfile" ~/.vimrc
#
# Globals:
#   None
# Arguments:
#   string_to_apply ($1) : String to apply.
#   config_file ($2)     : The file in which the string
#                          needs to be applied.
#   apply_at_top ($3?)   : If true puts the string at the top,
#                          otherwise append it (default true).
# Returns:
#   None
# Output:
#   None
#######################################
function apply(){
    local string_to_apply=$1
    local config_file=$2
    local apply_at_top=$3
    check_not_null $string_to_apply
    check_not_null $config_file

    if [ ! -e "$config_file" ]
    then
        local dirp=$(dirname "$config_file")
        mkdir -p $dirp
        touch "$config_file"
    fi

    local putfirst=true
    [ -z "$apply_at_top" ] || putfirst=$apply_at_top

    local original=$(grep -F -x -v "$string_to_apply" "$config_file")
    if $putfirst
    then
        echo -e "$string_to_apply\n$original" > "$config_file"
    else
        echo -e "$original\n$string_to_apply" > "$config_file"
    fi
}

#######################################
# Check if a string is applied to a file.
#
# Globals:
#   None
# Arguments:
#   string_to_apply ($1): String to apply
#   config_file ($2)    : The file in which the string
#                         needs to be applied
# Returns:
#   0                   : If $string_to_apply is matching
#                         a line in $config_file file.
#   1                   : If $string_to_apply is not available
#                         in $config_file file.
#   2                   : If $config_file file does not exist.
# Output:
#   None
#######################################
function is_applied(){
    local string_to_apply=$1
    local config_file=$2
    check_not_null $string_to_apply
    check_not_null $config_file

    grep -q -F -x "$string_to_apply" "$config_file"
}

#######################################
# Unapply a string to a file.
# The function is idempotent, so calling this function multiple
# times will remove the string entirely and if the string does not exist
# it will return successfully.
# If $config_file does not exist, the function will
# return successfully.
#
# Globals:
#   None
# Arguments:
#   string_to_apply ($1): String to apply
#   config_file ($2)    : The file in which the string
#                         needs to be applied
# Returns:
#   None
# Output:
#   None
#######################################
function unapply(){
    local string_to_apply=$1
    local config_file=$2
    check_not_null $string_to_apply
    check_not_null $config_file

    [ ! -e "$config_file" ] && return

    local original=$(grep -F -x -v "$string_to_apply" "$config_file")
    echo -e "$original" > $config_file
}

#######################################
# Simplify the use of the apply() function.
# User can apply a certain config file to a (well known) program without knowing
# about the syntax used to import the config file.
# All available programs are defined in the variable CONFIG_FILES.
#
# The function is idempotent, so calling this function multiple
# times will apply the string once.
# If the program config file does not exist, the function will create the file and all its
# parent directories (if needed).
#
# Example of usage:
#     link vim $HOME/myvimrc
#
# Globals:
#   HOME (RO)                  : The program config files are located in HOME.
# Arguments:
#   program ($1)               : Can be one of the keys in CONFIG_FILES
#   config_file_to_apply ($2)  : The file used to link to the program
#   apply_at_top ($3?)         : If true will put the line
#                                at the top of the file (default true).
# Returns:
#   33                         : If program does not exist
# Output:
#   None
#######################################
function link() {
    local program=$1
    local config_file_to_apply=$2
    local apply_at_top=$3
    check_not_null $program
    check_not_null $config_file_to_apply

    local config_file=${CONFIG_FILES[$program]}
    [ -z "$config_file" ] && { error "The program $program does not exist" ; return 33; }
    local source_line=${SOURCE_LINES[$program]}
    [ -z "$source_line" ] && { error "The program $program does not exist" ; return 33; }

    apply "${source_line/\{\}/$config_file_to_apply}" "$config_file" $apply_at_top
}

#######################################
# Simplify the use of the unapply() function.
# User can unapply a certain config file to a (well known) program without knowing
# about the syntax used to import the config file.
# All available programs are defined in the variable CONFIG_FILES.
#
# The function is idempotent, so calling this function multiple
# times will remove the string entirely and if the string does not exist
# it will return successfully.
# If the program config file does not exist, the function will
# return successfully.
#
# Example of usage:
#     unlink vim $HOME/myvimrc
#
# Globals:
#   HOME (RO)                  : The program config files are located in HOME.
# Arguments:
#   program ($1)               : Can be one of the keys in CONFIG_FILES
#   config_file_to_apply ($2)  : The file used to link to the program
#   apply_at_top ($3)          : If true will put the line
#                                at the top of the file (default true).
# Returns:
#   33                         : If program does not exist
#######################################
function unlink() {
    local program=$1
    local config_file_to_apply=$2
    check_not_null $program
    check_not_null $config_file_to_apply

    local config_file=${CONFIG_FILES[$program]}
    [ -z "$config_file" ] && { error "The program $program does not exist" ; return 33; }
    local source_line=${SOURCE_LINES[$program]}
    [ -z "$source_line" ] && { error "The program $program does not exist" ; return 33; }

    unapply "${source_line/\{\}/$config_file_to_apply}" "$config_file"
}

#######################################
# Link binary file to PATH variable by creating a symlink to the default
# $PEARL_HOME/bin directory.
#
# The function is idempotent, so calling this function multiple
# times will link the binary once.
#
# If $binary_path does not exist, the function will fail.
#
# Example of usage:
#    link_to_path "~/mybinaryfile"
#
# Globals:
#   PEARL_HOME (RO)      : Used to locate $PEARL_HOME/bin.
# Arguments:
#   binary_path ($1)     : The path of the binary file.
# Returns:
#   NO_FILE_OR_DIRECTORY : $binary_path does not exist.
#   0                    : Successfully linked.
# Output:
#   None
#######################################
function link_to_path() {
    local binary_path=$1
    check_not_null ${binary_path}

    [[ ! -e "${binary_path}" ]] \
        && { error "The path $binary_path does not exist" ; return $NO_FILE_OR_DIRECTORY; }

    unlink_from_path "${binary_path}"
    ln -s "${binary_path}" "${PEARL_HOME}/bin"

    return 0
}

#######################################
# Unlink binary file to PATH variable by removing the symlink from the default
# $PEARL_HOME/bin directory.
#
# The function is idempotent, so calling this function multiple
# times will unlink the binary once.
#
# If the symlink is broken, the symlink will be deleted.
# If the symlink corresponds to a different source binary path
# from $binary_path, the symlink will not be deleted.
#
# Example of usage:
#    unlink_from_path "~/mybinaryfile"
#
# Globals:
#   PEARL_HOME (RO)      : Used to locate $PEARL_HOME/bin.
# Arguments:
#   binary_path ($1)     : The path of the binary file.
# Returns:
#   0                    : Successfully unlinked.
#   36                   : Symlink exists on a differt source file.
# Output:
#   None
#######################################
function unlink_from_path() {
    local binary_path=$1
    check_not_null ${binary_path}

    local binary_path=$(readlink -f "${binary_path}")
    local binary_name=$(basename "$binary_path")
    if [[ -e ${PEARL_HOME}/bin/${binary_name}  ]]
    then
        local existing_path=$(readlink -f "${PEARL_HOME}/bin/${binary_name}")

        [[ "$existing_path" != "$binary_path" ]] \
            && { warn "Could not unlink: Symlink ${PEARL_HOME}/bin/${binary_name} already exists from source ${existing_path} which is different from $binary_path"; return 36; }
    fi
    [[ -L ${PEARL_HOME}/bin/${binary_name} ]] && rm -f ${PEARL_HOME}/bin/${binary_name}

    return 0
}
