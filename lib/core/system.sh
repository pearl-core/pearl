# This module contains all functionalities needed for
# handling the pearl core system.
#
# Dependencies:
# - lib/utils/utils.sh
# - lib/core/package.sh
#
# vim: ft=sh

GIT=git

#######################################
# Initialize the Pearl environment by
# setting up the PEARL_HOME configurations.
#
# Globals:
#   PEARL_HOME (RO)   :  The Pearl user configuration location.
#   HOME (RO)         :  The user home location.
#   PEARL_ROOT (RO)   :  Pearl program configuration location.
# Arguments:
#   None
# Returns:
#   0
# Output:
#   Logging information.
#######################################
function pearl_init(){
    bold_cyan; echo -n "* "; normal; echo "Creating Pearl configuration in $PEARL_HOME"
    mkdir -p $PEARL_HOME/bin
    mkdir -p $PEARL_HOME/packages
    mkdir -p $PEARL_HOME/repos
    mkdir -p $PEARL_HOME/tmp
    mkdir -p $PEARL_HOME/var

    [ -e "$PEARL_HOME/bin/pearl" ] && rm -f "$PEARL_HOME/bin/pearl"
    ln -s "$PEARL_ROOT/bin/pearl" "$PEARL_HOME/bin"

    [ -e "$PEARL_HOME/pearl.conf" ] || cp "$PEARL_ROOT/etc/pearl.conf.template" "$PEARL_HOME/pearl.conf"

    apply "source ${PEARL_ROOT}/boot/sh/pearl.sh" ${HOME}/.bashrc
    apply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.bashrc
    bold_cyan; echo -n "* "; normal; echo "Activated Pearl for Bash"

    apply "source ${PEARL_ROOT}/boot/sh/pearl.sh" ${HOME}/.zshrc
    apply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.zshrc
    bold_cyan; echo -n "* "; normal; echo "Activated Pearl for Zsh"

    apply "source ${PEARL_ROOT}/boot/fish/pearl.fish" ${HOME}/.config/fish/config.fish
    apply "set -x PEARL_ROOT ${PEARL_ROOT}" ${HOME}/.config/fish/config.fish
    bold_cyan; echo -n "* "; normal; echo "Activated Pearl for Fish shell"

    link vim "${PEARL_ROOT}/boot/vim/pearl.vim"
    bold_cyan; echo -n "* "; normal; echo "Activated Pearl for Vim editor"

    link emacs "${PEARL_ROOT}/boot/emacs/pearl.el"
    bold_cyan; echo -n "* "; normal; echo "Activated Pearl for Emacs editor"

    echo
    info "Done! Open a new terminal and have fun!"
    echo
    info "To get the list of Pearl packages available:"
    echo "    >> pearl list"
}

#######################################
# Remove completely the Pearl environment.
#
# Globals:
#   PEARL_HOME (RO)   :  The Pearl user configuration location.
#   HOME (RO)         :  The user home location.
#   PEARL_ROOT (RO)   :  Pearl program configuration location.
# Arguments:
#   None
# Returns:
#   0
# Output:
#   Logging information.
#######################################
function pearl_remove(){
    cd $PEARL_ROOT
    if ask "Are you sure to REMOVE all the Pearl packages in $PEARL_HOME folder?" "N"
    then
        get_list_installed_packages
        for pkgname in "${RESULT[@]}"
        do
            pearl_package_remove $pkgname
        done
        unset RESULT
    fi
    if ask "Are you sure to REMOVE all the Pearl hooks?" "N"
    then
        unapply "source ${PEARL_ROOT}/boot/sh/pearl.sh" ${HOME}/.bashrc
        unapply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.bashrc
        echo "* Deactivated Pearl for Bash"

        unapply "source ${PEARL_ROOT}/boot/sh/pearl.sh" ${HOME}/.zshrc
        unapply "export PEARL_ROOT=${PEARL_ROOT}" ${HOME}/.zshrc
        echo "* Deactivated Pearl for Zsh"

        unapply "source ${PEARL_ROOT}/boot/fish/pearl.fish" ${HOME}/.config/fish/config.fish
        unapply "set -x PEARL_ROOT ${PEARL_ROOT}" ${HOME}/.config/fish/config.fish
        echo "* Deactivated Pearl for Fish shell"

        unlink vim "${PEARL_ROOT}/boot/vim/pearl.vim"
        echo "* Deactivated Pearl for Vim editor"

        unlink emacs "${PEARL_ROOT}/boot/emacs/pearl.el"
        echo "* Deactivated Pearl for Emacs editor"
    fi
    if ask "Are you sure to REMOVE the Pearl config $PEARL_HOME directory (NOT RECOMMENDED)?" "N"
    then
       rm -rf $PEARL_HOME
    fi
}

#######################################
# Update the Pearl environment.
#
# Globals:
#   GIT (RO)         :  The git command.
#   PEARL_ROOT (RO)  :  Pearl program configuration location.
# Arguments:
#   None
# Returns:
#   0
# Output:
#   Logging information.
#######################################
function pearl_update(){
    cd $PEARL_ROOT
    bold_cyan; echo -n "* "; normal; echo "Updating Pearl script"
    $GIT fetch --quiet --all
    $GIT reset --quiet --hard origin/master

    get_list_installed_packages
    for pkgname in "${RESULT[@]}"
    do
        pearl_package_update $pkgname
    done
    unset RESULT
}

