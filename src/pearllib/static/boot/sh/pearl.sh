# Usage: source pearl OR in Bash: bash --rcfile pearl
# vim: set ft=sh ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
export PEARL_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}/pearl
# Fallback to a default temp directory if tty does not work
if tty -s
then
    export PEARL_TEMPORARY=${PEARL_HOME}/tmp/$(tty)
else
    export PEARL_TEMPORARY=${PEARL_HOME}/tmp/default-tty
fi
mkdir -p ${PEARL_TEMPORARY}

if [[ $PATH != *"${PEARL_HOME}/bin"* ]]
then
    PATH=$PATH:${PEARL_HOME}/bin
fi

################################# MAIN ##############################
# In ZSH 5.0.0 double square brackets do not work for checking empty directories
if [ "$(ls -A ${PEARL_HOME}/packages)" ]; then
    # Do not use ls command for listing the directories as gives few troubles
    for repopath in "${PEARL_HOME}/packages"/*
    do
        reponame="${repopath/*\//}"
        # In ZSH 5.0.0 double square brackets do not work for checking empty directories
        [ "$(ls -A ${PEARL_HOME}/packages/${reponame})" ] || continue

        # Do not use ls command for listing the directories as gives few troubles
        for pkgpath in "${PEARL_HOME}/packages/${reponame}"/*
        do
            pkgname="${pkgpath/*\//}"

            PEARL_PKGDIR=${PEARL_HOME}/packages/${reponame}/${pkgname}
            PEARL_PKGVARDIR=${PEARL_HOME}/var/${reponame}/${pkgname}
            PEARL_PKGNAME=${pkgname}
            PEARL_PKGREPONAME=${reponame}

            if [[ -e ${PEARL_PKGDIR}/pearl-config/config.sh ]]; then
                source ${PEARL_PKGDIR}/pearl-config/config.sh
            fi
            if [[ -n "$BASH" ]] && [[ -e ${PEARL_PKGDIR}/pearl-config/config.bash ]]; then
                source ${PEARL_PKGDIR}/pearl-config/config.bash
            fi
            if [[ -n "$ZSH_NAME" ]] && [[ -e ${PEARL_PKGDIR}/pearl-config/config.zsh ]]; then
                source ${PEARL_PKGDIR}/pearl-config/config.zsh
            fi
            unset PEARL_PKGDIR PEARL_PKGVARDIR PEARL_PKGNAME PEARL_PKGREPONAME
        done
    done

    unset repopath reponame pkgpath pkgname
fi

function pearl-source() {
    [[ -n "$ZSH_NAME" ]] && source ${HOME}/.zshrc
    [[ -n "$BASH" ]] && source ${HOME}/.bashrc
    return 0
}

