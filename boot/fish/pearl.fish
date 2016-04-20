# Pearl script for fish shell
# Usage: source pearl.fish
# vim: set ft=fish ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
if [ -z "$PEARL_ROOT" ]
    echo "Error: PEARL_ROOT environment variable does not exist."
	exit 1
end

if [ ! -d "$PEARL_ROOT" ]
    echo "Error: PEARL_ROOT directory '$PEARL_ROOT' does not exist."
    exit 2
end

set -x PEARL_HOME $HOME/.config/pearl
set -x PEARL_TEMPORARY $PEARL_HOME/tmp/(tty)
mkdir -p $PEARL_TEMPORARY

set PATH $PEARL_ROOT/bin $PATH
set MANPATH $MANPATH $PEARL_ROOT/man

################################# MAIN ##############################
for reponame in (ls $PEARL_HOME/packages)
	for pkgname in (ls $PEARL_HOME/packages/$reponame)
        set PEARL_PKGDIR "$PEARL_HOME/packages/$reponame/$pkgname"
		if [ -e $PEARL_PKGDIR/pearl-metadata/config.fish ]
			source $PEARL_PKGDIR/pearl-metadata/config.fish
		end
		set -e PEARL_PKGDIR
	end
end

trap "source $PEARL_ROOT/boot/fish/pearl.fish" USR1
