#!/usr/bin/fish


if [ -z $argv[1] ]
    echo "ERROR: To run the integ tests you must specify the Pearl location."
    echo "For instance: $0 ~/.local/share/pearl"
    exit 33
end

source $HOME/.config/fish/config.fish

set -x PEARL_ROOT $argv[1]
set -x HOME (mktemp -d -t pearl-user-home.XXXXXXX)
set -x PEARL_HOME "$HOME/.config/pearl"
set -x PATH $PEARL_GIT_ROOT/bin $PATH

function pearl_remove_home --on-process-exit %self
    rm -fr $HOME
end

function get_all_packages
    ls $PEARL_HOME/packages/pearl
end

function info
    set_color white
    echo $argv
    set_color normal
end

function die
    set_color red
    echo $argv
    set_color normal
    exit 1
end

[ ! -e $PEARL_HOME ]; or die "$PEARL_HOME exists"
pearl init; or die "Error on pearl install"
[ -d $PEARL_HOME/packages ]; or die "$PEARL_HOME/packages does not exist after install"
[ -d $PEARL_HOME/repos ]; or die "$PEARL_HOME/repos does not exist after install"

source $PEARL_ROOT/boot/fish/pearl.fish; or die "Error on sourcing pearl.fish"
[ -e $PEARL_ROOT ]; or die "$PEARL_ROOT does not exist"
[ -e $PEARL_HOME ]; or die "$PEARL_HOME does not exist"
[ -e $PEARL_TEMPORARY ]; or die "$PEARL_TEMPORARY does not exist"

pearl list

info Install ALL pearl packages
for package in (bash -c 'declare -A PEARL_PACKAGES; source $PEARL_HOME/repos/*/repo.conf; for p in ${!PEARL_PACKAGES[@]}; do echo $p; done;')
    yes "" | pearl install $package; or die "Error on pearl install $package"
    [ -d "$PEARL_HOME/packages/pearl/$package" ]; or die "$PEARL_HOME/packages/pearl/$package does not exist"
    [ -d "$PEARL_HOME/packages/pearl/$package/module" ]; and  bash -c '[ ! "(ls -A $PEARL_HOME/packages/pearl/$package/module)" ]'; and die "$PEARL_HOME/packages/pearl/$package/module exists but it is empty"
end

info Update ALL Pearl packages
for package in (get_all_packages)
    yes "" | pearl update $package; or die "Error on pearl update $package"
end

pearl list

info Remove ALL pearl packages
for package in (get_all_packages)
    pearl remove $package; or die "Error on pearl remove $package"
    [ -d "$PEARL_HOME/packages/pearl/$package" ]; and die "$PEARL_HOME/packages/pearl/$package still exists"
end

yes | pearl remove; or die "Error on pearl remove"
[ ! -e $PEARL_HOME ]; or echo "$PEARL_HOME exists after remove it"
