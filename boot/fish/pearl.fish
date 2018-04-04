# Pearl script for fish shell
# Usage: source pearl.fish
# vim: set ft=fish ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
if [ -z "$PEARL_ROOT" ]
    echo "Error: PEARL_ROOT environment variable does not exist." 1>&2
	exit 1
end

if [ ! -d "$PEARL_ROOT" ]
    echo "Error: PEARL_ROOT directory '$PEARL_ROOT' does not exist." 1>&2
    exit 2
end

set -x PEARL_HOME $HOME/.config/pearl
# Fallback to a default temp directory if tty does not work
if tty -s
    set -x PEARL_TEMPORARY $PEARL_HOME/tmp/(tty)
else
    set -x PEARL_TEMPORARY $PEARL_HOME/tmp/default-tty
end
mkdir -p $PEARL_TEMPORARY

if not contains $PEARL_HOME/bin $PATH
    set PATH $PATH $PEARL_HOME/bin
end
if not contains $PEARL_ROOT/man $MANPATH
    set MANPATH $MANPATH $PEARL_ROOT/man
end

################################# MAIN ##############################
for reponame in (ls $PEARL_HOME/packages)
    for pkgname in (ls $PEARL_HOME/packages/$reponame)
        set PEARL_PKGDIR "$PEARL_HOME/packages/$reponame/$pkgname"
        set PEARL_PKGVARDIR "$PEARL_HOME/var/$reponame/$pkgname"
        set PEARL_PKGREPONAME "$reponame"
        set PEARL_PKGNAME "$pkgname"

        # TODO pearl-metadata directory is meant to be deprecated in the future versions
        if [ -e $PEARL_PKGDIR/pearl-metadata/config.fish ]
            source $PEARL_PKGDIR/pearl-metadata/config.fish
        end

        if [ -e $PEARL_PKGDIR/pearl-config/config.fish ]
            source $PEARL_PKGDIR/pearl-config/config.fish
        end
        set -e PEARL_PKGDIR
        set -e PEARL_PKGVARDIR
		set -e PEARL_PKGNAME
		set -e PEARL_PKGREPONAME
    end
end

# trap for OSX works differently due to the BSD getopt.
# With function things works fine with both GNU and BSD version:
# https://github.com/fish-shell/fish-shell/issues/607
function source_pearl
    source $PEARL_ROOT/boot/fish/pearl.fish
end

trap source_pearl USR1
