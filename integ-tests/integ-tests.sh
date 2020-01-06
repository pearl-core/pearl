#!/usr/bin/env bash

set -ex

unset PEARL_ROOT
unset PEARL_HOME
unset PEARL_TEMPORARY

if [[ -z "$1" ]]
then
    echo "ERROR: To run the integ tests you must specify the Pearl location."
    echo "For instance: $0 ~/.local/share/pearl"
    exit 33
fi

export PEARL_ROOT="$1"
export PEARL_HOME="${HOME}/.config/pearl"
export PATH=$PEARL_ROOT/bin:$PATH

# In later versions of Travis the signals are not recognized in zsh: QUIT TERM KILL ABRT
# https://travis-ci.org/pearl-core/pearl/jobs/540316660
trap "rm -rf ${PEARL_HOME}/" EXIT

function get_all_packages(){
    ls ${PEARL_HOME}/packages/pearl
}

function info(){
    # $@: msg (mandatory) - str: Message to print
    echo -e "\033[1;37m$@\033[0m"
}

[[ ! -d ${PEARL_HOME} ]]
pearl init
[[ -d ${PEARL_HOME}/packages ]] || { echo "$PEARL_HOME/packages does not exist after install"; exit 1; }
[[ -d ${PEARL_HOME}/repos ]] || { echo "$PEARL_HOME/repos does not exist after install"; exit 2; }

[[ -n "$ZSH_NAME" ]] && source ${HOME}/.zshrc
[[ -n "$BASH" ]] && source ${HOME}/.bashrc

[[ -d "$PEARL_ROOT" ]] || { echo "$PEARL_ROOT does not exist"; exit 3; }
[[ -d "$PEARL_HOME" ]] || { echo "$PEARL_HOME does not exist"; exit 4; }
[[ -d "$PEARL_TEMPORARY" ]] || { echo "$PEARL_TEMPORARY does not exist"; exit 5; }

pearl list

info Install ALL pearl packages
for package in $(pearl list --package-only)
do
    pearl --verbose --no-confirm emerge ${package}
    [[ -d "$PEARL_HOME/packages/$package" ]] || { echo "$PEARL_HOME/packages/$package does not exist"; exit 6; }
    if [[ -d "$PEARL_HOME/packages/$package/module" ]]
    then
        [[ "$(ls -A ${PEARL_HOME}/packages/${package}/module)" ]] || { echo "$PEARL_HOME/packages/pearl/$package/module exists but it is empty"; exit 7; }
    fi
done

info Update ALL Pearl packages
for package in $(get_all_packages)
do
    pearl --verbose --no-confirm update ${package}
done

pearl list

info Remove ALL pearl packages
for package in $(get_all_packages)
do
    pearl --verbose --no-confirm remove ${package}
    [[ -d "$PEARL_HOME/packages/pearl/$package" ]] && { echo "$PEARL_HOME/packages/pearl/$package still exists"; exit 8; }
done

yes | pearl remove
[[ ! -e ${PEARL_HOME} ]] || echo "$PEARL_HOME exists after remove it"
