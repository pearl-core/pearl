#!/usr/bin/env bash

set -e

unset PEARL_ROOT
unset PEARL_HOME
unset PEARL_TEMPORARY

if [ -z "$1" ]
then
    echo "ERROR: To run the integ tests you must specify the Pearl location."
    echo "For instance: $0 ~/.local/share/pearl"
    exit 33
fi

[ -n "$ZSH_NAME" ] && source ${HOME}/.zshrc
[ -n "$BASH" ] && source ${HOME}/.bashrc

export PEARL_ROOT="$1"
export HOME=$(TMPDIR=/tmp mktemp -d -t pearl-user-home.XXXXXXX)
export PEARL_HOME="${HOME}/.config/pearl"
export PATH=$PEARL_ROOT/bin:$PATH

trap "rm -rf ${HOME}/" EXIT QUIT TERM KILL ABRT

function get_all_packages(){
    ls $PEARL_HOME/packages/pearl
}

function info(){
    # $@: msg (mandatory) - str: Message to print
    echo -e "\033[1;37m$@\033[0m"
}

[ ! -d $PEARL_HOME ]
pearl init
[ -d $PEARL_HOME/packages ] || { echo "$PEARL_HOME/packages does not exist after install"; exit 1; }
[ -d $PEARL_HOME/repos ] || { echo "$PEARL_HOME/repos does not exist after install"; exit 2; }

source $PEARL_ROOT/boot/sh/pearl.sh
[ -d "$PEARL_ROOT" ] || { echo "$PEARL_ROOT does not exist"; exit 3; }
[ -d "$PEARL_HOME" ] || { echo "$PEARL_HOME does not exist"; exit 4; }
[ -d "$PEARL_TEMPORARY" ] || { echo "$PEARL_TEMPORARY does not exist"; exit 5; }

pearl list

info Install ALL pearl packages
for package in $(bash -c 'declare -A PEARL_PACKAGES; source $PEARL_HOME/repos/*/repo.conf; echo ${!PEARL_PACKAGES[@]};')
do
    yes "" | pearl install $package
    [ -d "$PEARL_HOME/packages/pearl/$package" ] || { echo "$PEARL_HOME/packages/pearl/$package does not exist"; exit 6; }
    [ -d "$PEARL_HOME/packages/pearl/$package/module" ] && [ ! "$(ls -A $PEARL_HOME/packages/pearl/$package/module)" ] && { echo "$PEARL_HOME/packages/pearl/$package/module exists but it is empty"; exit 7; }
done

info Update ALL Pearl packages
for package in $(get_all_packages)
do
    yes "" | pearl update $package
done

pearl list

info Remove ALL pearl packages
for package in $(get_all_packages)
do
    pearl remove $package
    [ -d "$PEARL_HOME/packages/pearl/$package" ] && { echo "$PEARL_HOME/packages/pearl/$package still exists"; exit 8; }
done

yes | pearl remove
[ ! -e $PEARL_HOME ] || echo "$PEARL_HOME exists after remove it"
