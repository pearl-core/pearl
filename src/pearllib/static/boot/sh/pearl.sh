# Usage: source pearl OR in Bash: bash --rcfile pearl
# vim: set ft=sh ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
export PEARL_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}/pearl

if [[ $PATH != *"${PEARL_HOME}/bin"* ]]
then
    PATH=$PATH:${PEARL_HOME}/bin
fi

################################# MAIN ##############################
for pkgfullname in $(pearl list --dependency-tree --package-only --installed-only)
do
    pkgname="${pkgfullname/*\//}"
    reponame="${pkgfullname/\/*/}"

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
    unset reponame pkgname
done
unset pkgfullname

function pearl-source() {
    source "${PEARL_HOME}/boot/sh/pearl.sh"
    return 0
}

