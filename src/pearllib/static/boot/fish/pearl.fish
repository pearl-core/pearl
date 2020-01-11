# Pearl script for fish shell
# Usage: source pearl.fish
# vim: set ft=fish ts=4 sw=4 noet:

####################### VARIABLES & IMPORTS ############################
set -x PEARL_HOME $HOME/.local/share/pearl

if not contains $PEARL_HOME/bin $PATH
    set PATH $PATH $PEARL_HOME/bin
end

################################# MAIN ##############################
for reponame in $PEARL_HOME/packages/*
    set reponame (basename $reponame)
    for pkgname in $PEARL_HOME/packages/$reponame/*
        set pkgname (basename $pkgname)
        set PEARL_PKGDIR "$PEARL_HOME/packages/$reponame/$pkgname"
        set PEARL_PKGVARDIR "$PEARL_HOME/var/$reponame/$pkgname"
        set PEARL_PKGREPONAME "$reponame"
        set PEARL_PKGNAME "$pkgname"

        if [ -e $PEARL_PKGDIR/pearl-config/config.fish ]
            source $PEARL_PKGDIR/pearl-config/config.fish
        end
        set -e PEARL_PKGDIR
        set -e PEARL_PKGVARDIR
        set -e PEARL_PKGNAME
        set -e PEARL_PKGREPONAME
    end
end

function pearl-source
    source "$PEARL_HOME/boot/fish/pearl.fish"
end
