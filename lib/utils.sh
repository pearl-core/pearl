#!/bin/sh

echoerr() {
    # $@: msg (mandatory) - str: Message to print
    echo "$@" 1>&2;
}

function die(){
    # $@: msg (mandatory) - str: Message to print
    error $@
    exit 1
}

function error(){
    # $@: msg (mandatory) - str: Message to print
    echoerr -e "\033[1;31m$@\033[0m"
}

function warn(){
    # $@: msg (mandatory) - str: Message to print
    echoerr -e "\033[1;33m$@\033[0m"
}

function info(){
    # $@: msg (mandatory) - str: Message to print
    echo -e "\033[1;37m$@\033[0m"
}

function bold_white(){
    echo -ne "\033[1;37m"
}

function bold_cyan(){
    echo -ne "\033[1;36m"
}

function bold_red(){
    echo -ne "\033[1;35m"
}

function normal(){
    echo -ne "\033[0m"
}

function ask(){
    # $1: question string
    # $2: default value - can be either Y, y, N, n (by default Y)

    local default="Y"
    [ -z $2 ] || default=$(echo "$2" | tr '[:lower:]' '[:upper:]')

    local other="n"
    [ "$default" == "N" ] && other="y"

    local prompt=$(info "$1 (${default}/${other})> ")

    local res="none"
    while [ "$res" != "Y" ] && [ "$res" != "N"  ] && [ "$res" != "" ];
    do
        read -p "$prompt" res
        res=$(echo "$res" | tr '[:lower:]' '[:upper:]')
    done

    [ "$res" == "" ] && res="$default"

    if [ "$res" == "Y" ]
    then
        return 0
    else
        return 1
    fi

}


function apply(){
    # $1: String to apply
    # $2: File path in which the string must be applied
    # $3: bool - put string at the beginning (default true)
    #
    # If the file doesn't exist create it and append the line

    if [ ! -e "$2" ]
    then
        local dirp=$(dirname $2)
        mkdir -p $dirp
        touch "$2"
    fi

    local putfirst=true
    [ -z $3 ] || putfirst=$3

    local original=$(grep -F -x -v "$1" "$2")
    if $putfirst
    then
        echo -e "$1\n$original" > $2
    else
        echo -e "$original\n$1" > $2
    fi
}

# Returns the exit status:
# - 0 if $1 is matching a line in $2 file.
# - 1 if $1 is not available in $2 file.
# - 2 if $2 file does not exist.
function is_applied(){
    grep -q -F -x "$1" "$2"
}

function unapply(){
    [ ! -e "$2" ] && return

    local original=$(grep -F -x -v "$1" "$2")
    echo -e "$original" > $2
}
