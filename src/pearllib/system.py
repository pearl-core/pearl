import os
from argparse import Namespace
from pathlib import Path

import pkg_resources
import shutil

from pearllib.messenger import messenger, Color
from pearllib.package import update_packages, remove_packages
from pearllib.pearlenv import PearlEnvironment
from pearllib.utils import apply, ask, unapply


def init_pearl(pearl_env: PearlEnvironment, _: Namespace):
    """
    Initializes the Pearl environment by setting up the PEARL_HOME files and directories and the `pearl.conf` file.
    """
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Setting up $PEARL_HOME directory as {pearl_env.home}'
    )

    (pearl_env.home / 'bin').mkdir(parents=True, exist_ok=True)
    (pearl_env.home / 'packages').mkdir(parents=True, exist_ok=True)
    (pearl_env.home / 'repos').mkdir(parents=True, exist_ok=True)
    (pearl_env.home / 'var').mkdir(parents=True, exist_ok=True)

    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))

    if (pearl_env.home / 'boot').exists():
        (pearl_env.home / 'boot').unlink()
    (pearl_env.home / 'boot').symlink_to(static / 'boot')

    if not pearl_env.config_filename.exists():
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Creating the Pearl configuration file {pearl_env.config_filename} from template $PEARL_HOME'
        )
        pearl_env.config_filename.parent.mkdir(parents=True, exist_ok=True)
        pearl_conf_template = static / 'templates/pearl.conf.template'
        shutil.copyfile(str(pearl_conf_template), str(pearl_env.config_filename))

    apply(
        f"source {pearl_env.home}/boot/sh/pearl.sh",
        f"{os.environ['HOME']}/.bashrc"
    )
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Activated Pearl for Bash'
    )

    apply(
        f"source {pearl_env.home}/boot/sh/pearl.sh",
        f"{os.environ['HOME']}/.zshrc"
    )
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Activated Pearl for Zsh'
    )

    apply(
        f"source {pearl_env.home}/boot/fish/pearl.fish",
        f"{os.environ['HOME']}/.config/fish/config.fish"
    )
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Activated Pearl for Fish shell'
    )

    apply(
        f"source {pearl_env.home}/boot/vim/pearl.vim",
        f"{os.environ['HOME']}/.vimrc"
    )
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Activated Pearl for Vim editor'
    )

    apply(
        f"(load-file \"{pearl_env.home}/boot/emacs/pearl.el\")",
        f"{os.environ['HOME']}/.emacs"
    )
    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Activated Pearl for Emacs editor'
    )

    messenger.info('')
    messenger.info("Done! Open a new terminal and have fun!")
    messenger.info('')
    messenger.info("To get the list of Pearl packages available:")
    messenger.print("    >> pearl list")


def remove_pearl(pearl_env: PearlEnvironment, args: Namespace):
    """
    Removes completely the Pearl environment.
    """
    for repo_name, repo_packages in pearl_env.packages.items():
        if ask(
            f"Are you sure to REMOVE all the installed packages in {repo_name} repository?",
            yes_as_default_answer=False, no_confirm=args.no_confirm
        ):
            package_list = []
            for _, package in repo_packages.items():
                if package.is_installed():
                    package_list.append(package)
            args.packages = package_list
            remove_packages(pearl_env, args=args)

    if ask(
        "Are you sure to REMOVE all the Pearl hooks?",
        yes_as_default_answer=False, no_confirm=args.no_confirm
    ):
        unapply(
            f"source {pearl_env.home}/boot/sh/pearl.sh",
            f"{os.environ['HOME']}/.bashrc"
        )
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Deactivated Pearl for Bash'
        )

        unapply(
            f"source {pearl_env.home}/boot/sh/pearl.sh",
            f"{os.environ['HOME']}/.zshrc"
        )
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Deactivated Pearl for Zsh'
        )

        unapply(
            f"source {pearl_env.home}/boot/fish/pearl.fish",
            f"{os.environ['HOME']}/.config/fish/config.fish"
        )
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Deactivated Pearl for Fish shell'
        )

        unapply(
            f"source {pearl_env.home}/boot/vim/pearl.vim",
            f"{os.environ['HOME']}/.vimrc"
        )
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Deactivated Pearl for Vim editor'
        )

        unapply(
            f"(load-file \"{pearl_env.home}/boot/emacs/pearl.el\")",
            f"{os.environ['HOME']}/.emacs"
        )
        messenger.print(
            f'{Color.CYAN}* {Color.NORMAL}Deactivated Pearl for Emacs editor'

        )

    if ask(
        "Are you sure to REMOVE the Pearl config $PEARL_HOME directory (NOT RECOMMENDED)?",
        yes_as_default_answer=False, no_confirm=args.no_confirm
    ):
        shutil.rmtree(str(pearl_env.home))


def update_pearl(pearl_env: PearlEnvironment, args: Namespace):
    """Updates the Pearl environment."""
    package_list = []
    for repo_name, repo_packages in pearl_env.packages.items():
        for _, package in repo_packages.items():
            if package.is_installed():
                package_list.append(package)
    args.packages = package_list
    update_packages(pearl_env, args=args)
