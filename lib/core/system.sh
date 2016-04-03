# This module contains all functionalities needed for
# handling the pearl core system.
#
# Dependencies:
# - lib/utils.sh
# - lib/core/package.sh
#
# vim: ft=sh

GIT=git

function pearl_install(){
    local PEARL_ROOT=$1
    [ ! -d "$PEARL_ROOT" ] && die "Error: Could not set PEARL_ROOT env because ${PEARL_ROOT} does not exist."

    # Shows information system
    echo ""
    command -v uname && uname -m
    cat /etc/*release
    echo ""

    mkdir -p $PEARL_HOME/repos
    mkdir -p $PEARL_HOME/packages

    cp "$PEARL_ROOT/etc/pearl.conf.template" "$PEARL_HOME/pearl.conf"

    apply "source ${PEARL_ROOT}/boot/pearl.sh" ${HOME}/.bashrc
    apply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.bashrc
    echo "* Activated Pearl for Bash"
    apply "source ${PEARL_ROOT}/boot/pearl.sh" ${HOME}/.zshrc
    apply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.zshrc
    echo "* Activated Pearl for Zsh"
    apply "source ${PEARL_ROOT}/boot/pearl.fish" ${HOME}/.config/fish/config.fish
    apply "set -x PEARL_ROOT ${PEARL_ROOT}" ${HOME}/.config/fish/config.fish
    echo "* Activated Pearl for Fish shell"
    apply "source ${PEARL_ROOT}/boot/vim/pearl.vim" ${HOME}/.vimrc
    echo "* Activated Pearl for Vim editor"
    info ""
    info "Done! Open a new terminal and have fun!"
    info ""
    info "To get the list of Pearl packages available:"
    echo "    >> pearl list"
}

function pearl_remove(){
    cd $PEARL_ROOT
    if ask "Are you sure to REMOVE all the Pearl packages in $PEARL_HOME folder?" "N"
    then
	pearl_load_repos
        for pkgname in $(get_list_installed_packages)
        do
            pearl_package_remove $pkgname
        done
    fi
    if ask "Are you sure to REMOVE all the Pearl hooks?" "N"
    then
        unapply "source ${PEARL_ROOT}/boot/pearl.sh" ${HOME}/.bashrc
        unapply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.bashrc
        echo "* Deactivated Pearl for Bash"
        unapply "source ${PEARL_ROOT}/boot/pearl.sh" ${HOME}/.zshrc
        unapply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.zshrc
        echo "* Deactivated Pearl for Zsh"
        unapply "source ${PEARL_ROOT}/boot/pearl.fish" ${HOME}/.config/fish/config.fish
        unapply "set -x PEARL_ROOT ${PEARL_ROOT}" ${HOME}/.config/fish/config.fish
        echo "* Deactivated Pearl for Fish shell"
        unapply "source ${PEARL_ROOT}/boot/vim/pearl.vim" ${HOME}/.vimrc
        echo "* Deactivated Pearl for Vim editor"
    fi
    if ask "Are you sure to REMOVE the Pearl config $PEARL_HOME directory (NOT RECOMMENDED)?" "N"
    then
       rm -rf $PEARL_HOME
    fi
}

function pearl_update(){
    cd $PEARL_ROOT
    $GIT fetch --all
    $GIT reset --hard origin/master

    pearl_load_repos
    for pkgname in $(get_list_installed_packages)
    do
        pearl_package_update $pkgname
    done
}

