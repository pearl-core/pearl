# Pearl script for fish shell
# Usage: source pearl.fish
# vim: set ft=fish ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
set -x PEARL_HOME $HOME/.local/share/pearl

if not contains $PEARL_HOME/bin $PATH
    set PATH $PATH $PEARL_HOME/bin
end

################################# MAIN ##############################
if command -v pearl > /dev/null
    for pkgfullname in (pearl list --dependency-tree --package-only --installed-only)
        set reponame (dirname $pkgfullname)
        set pkgname (basename $pkgfullname)
        set PEARL_PKGDIR "$PEARL_HOME/packages/$reponame/$pkgname"
        set PEARL_PKGVARDIR "$PEARL_HOME/var/$reponame/$pkgname"
        set PEARL_PKGREPONAME "$reponame"
        set PEARL_PKGNAME "$pkgname"

        if [ -e $PEARL_PKGDIR/pearl-config/config.fish ]
            if [ $PEARL_DEBUG ]
                echo "Running $PEARL_PKGDIR/pearl-config/config.fish..."
            end
            source $PEARL_PKGDIR/pearl-config/config.fish
        end
        set -e PEARL_PKGDIR
        set -e PEARL_PKGVARDIR
        set -e PEARL_PKGNAME
        set -e PEARL_PKGREPONAME
    end
else
    echo "Pearl error: Could not load pearl package config files. `pearl` executable not found. Please update the PATH variable first."
end

function pearl-source
    source "$PEARL_HOME/boot/fish/pearl.fish"
end
