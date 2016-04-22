# Usage: source pearl OR in Bash: bash --rcfile pearl
# vim: set ft=sh ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
[ -z "${PEARL_ROOT}" ] && { echo "Error: PEARL_ROOT environment variable does not exist."; return 1; }
[ ! -d "${PEARL_ROOT}" ] && { echo "Error: PEARL_ROOT directory '${PEARL_ROOT}' does not exist."; return 2; }

export PEARL_ROOT
export PEARL_HOME=${HOME}/.config/pearl
export PEARL_TEMPORARY=${PEARL_HOME}/tmp/$(tty)
mkdir -p ${PEARL_TEMPORARY}

PATH=${PEARL_ROOT}/bin:${PATH}
MANPATH=${MANPATH}:${PEARL_ROOT}/man

################################# MAIN ##############################
for reponame in $(ls ${PEARL_HOME}/packages/)
do
	for pkgname in $(ls ${PEARL_HOME}/packages/${reponame}/)
	do
		PEARL_PKGDIR=${PEARL_HOME}/packages/${reponame}/${pkgname}
		if [ -e ${PEARL_PKGDIR}/pearl-metadata/config.sh ]; then
			source ${PEARL_PKGDIR}/pearl-metadata/config.sh
		fi
		if [ -n "$BASH" ] && [ -e ${PEARL_PKGDIR}/pearl-metadata/config.bash ]; then
			source ${PEARL_PKGDIR}/pearl-metadata/config.bash
		fi
		if [ -n "$ZSH_NAME" ] && [ -e ${PEARL_PKGDIR}/pearl-metadata/config.zsh ]; then
			source ${PEARL_PKGDIR}/pearl-metadata/config.zsh
		fi
		unset PEARL_PKGDIR
	done
done

trap "source ${PEARL_ROOT}/boot/sh/pearl.sh" USR1
