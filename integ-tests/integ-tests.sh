#!/usr/bin/env bash

set -e

unset PEARL_HOME

if [[ -z "$1" ]]
then
    echo "ERROR: To run the integ tests you must specify the Pearl location."
    echo "For instance: $0 ~/.local/share/pearl"
    exit 33
fi

export PEARL_ROOT="$1"
PEARL_HOME="${HOME}/.local/share/pearl"

# Disabling vim since it hangs on OSX
# https://stackoverflow.com/questions/46432027/bash-kill-vim-when-vim-warning-output-not-to-a-terminal
mkdir -p "$HOME"/bin
export PATH=$HOME/bin:$PATH
cat <<EOF >> "$HOME/bin/vim"
#!/bin/sh
echo vim_mock \$@
exit
EOF
chmod +x "$HOME"/bin/vim

# In later versions of Travis the signals are not recognized in zsh: QUIT TERM KILL ABRT
# https://travis-ci.org/pearl-core/pearl/jobs/540316660
# shellcheck disable=SC2064
trap "rm -rf ${PEARL_HOME}/" EXIT

function info(){
    # $@: msg (mandatory) - str: Message to print
    echo -e "\033[1;37m$*\033[0m"
}

[[ ! -d ${PEARL_HOME} ]]
pearl init
[[ -d ${PEARL_HOME}/packages ]] || { echo "$PEARL_HOME/packages does not exist after install"; exit 1; }
[[ -d ${PEARL_HOME}/repos ]] || { echo "$PEARL_HOME/repos does not exist after install"; exit 2; }

# shellcheck disable=SC1091
source "${HOME}/.bashrc"

[[ -d "$PEARL_ROOT" ]] || { echo "$PEARL_ROOT does not exist"; exit 3; }
[[ -d "$PEARL_HOME" ]] || { echo "$PEARL_HOME does not exist"; exit 4; }

info "Creating a local pearl package"
pearl create mydotfiles "$HOME"/dotfiles
pearl search mydotfiles

echo -e "[alias]\n    cfg = config" > "$HOME"/dotfiles/gitconfig
cat > "$HOME"/dotfiles/pearl-config/hooks.sh <<EOF
post_install() {
    link git "\${PEARL_PKGDIR}/gitconfig"
    return 0
}

post_update() {
    post_install
}

pre_remove() {
    unlink git "\${PEARL_PKGDIR}/gitconfig"
    return 0
}
EOF

pearl install mydotfiles
git cfg -l

info "Listing all pearl packages"
pearl list

info Install ALL pearl packages
for package in local/mydotfiles pearl/sesaila pearl/txum pearl/dot-bash pearl/cmd
do
    pearl info ${package}
    pearl -vv --no-confirm emerge ${package}
    [[ -d "$PEARL_HOME/packages/$package" ]] || { echo "$PEARL_HOME/packages/$package does not exist"; exit 6; }
done

info Update ALL Pearl packages
pearl -vv --no-confirm update

info Test the Bash, Zsh and Fish boot files
export PEARL_DEBUG=1
bash -c 'source $HOME/.bashrc; set -e; pearl-source; pearl --help; [[ -n $PEARL_HOME ]]; which cmd'
zsh -c 'source $HOME/.zshrc; set -e; pearl-source; pearl --help; [[ -n $PEARL_HOME ]]; which cmd'
# shellcheck disable=SC2016
fish -c 'source $HOME/.config/fish/config.fish; pearl-source; pearl --help; [ $PEARL_HOME ]; which cmd'

pearl list

info Remove ALL pearl packages
# Remove only the packages for now
echo -e "y\ny\nn\nn\n" | pearl remove
# Double brackets do not work in ZSH when checking for empty directory
if [ "$(ls -A "$PEARL_HOME/packages/pearl/")" ]
then
    echo "$PEARL_HOME/packages/pearl/ is not empty but all the Pearl packages have been removed"
    exit 8
fi

# Remove everything now
yes | pearl remove
# Remove the created package
rm -rf "$HOME"/mydotfiles
[[ ! -e ${PEARL_HOME} ]] || echo "$PEARL_HOME exists after remove it"
