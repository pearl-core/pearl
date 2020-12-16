# Usage: source pearl OR in Bash: bash --rcfile pearl
# vim: set ft=sh ts=4 sw=4 noet:

####################### FUNCTIONS ############################
function add_to_path() {
    local pathname="$(realpath -m -s "$1")"
    if [[ $PATH != *"${pathname}"* ]]
    then
        export PATH=$PATH:${pathname}
    fi

    return 0
}

####################### VARIABLES & IMPORTS ############################
export PEARL_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}/pearl

add_to_path "${PEARL_HOME}/bin"

################################# MAIN ##############################
if command -v pearl > /dev/null; then
    for pkgfullname in $(pearl list --dependency-tree --package-only --installed-only)
    do
        pkgname="${pkgfullname/*\//}"
        reponame="${pkgfullname/\/*/}"

        PEARL_PKGDIR=${PEARL_HOME}/packages/${reponame}/${pkgname}
        PEARL_PKGVARDIR=${PEARL_HOME}/var/${reponame}/${pkgname}
        PEARL_PKGNAME=${pkgname}
        PEARL_PKGREPONAME=${reponame}

        if [[ -e ${PEARL_PKGDIR}/pearl-config/config.sh ]]; then
            if [[ -n ${PEARL_DEBUG} ]]; then
                echo "Running ${PEARL_PKGDIR}/pearl-config/config.sh..."
            fi
            source "${PEARL_PKGDIR}/pearl-config/config.sh"
        fi
        if [[ -n "$BASH" ]] && [[ -e ${PEARL_PKGDIR}/pearl-config/config.bash ]]; then
            if [[ -n ${PEARL_DEBUG} ]]; then
                echo "Running ${PEARL_PKGDIR}/pearl-config/config.bash..."
            fi
            source "${PEARL_PKGDIR}/pearl-config/config.bash"
        fi
        if [[ -n "$ZSH_NAME" ]] && [[ -e ${PEARL_PKGDIR}/pearl-config/config.zsh ]]; then
            if [[ -n ${PEARL_DEBUG} ]]; then
                echo "Running ${PEARL_PKGDIR}/pearl-config/config.zsh..."
            fi
            source "${PEARL_PKGDIR}/pearl-config/config.zsh"
        fi
        unset PEARL_PKGDIR PEARL_PKGVARDIR PEARL_PKGNAME PEARL_PKGREPONAME
        unset reponame pkgname
    done
    unset pkgfullname
else
    echo "Pearl error: Could not load pearl package config files. `pearl` executable not found. Please update the PATH variable first."
fi

function pearl-source() {
    source "${PEARL_HOME}/boot/sh/pearl.sh"
    return 0
}

