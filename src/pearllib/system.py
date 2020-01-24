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
        '{cyan}* {normal}Setting up $PEARL_HOME directory as {home}'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            home=pearl_env.home,
        )
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
            '{cyan}* {normal}Creating the Pearl configuration file {configfile} from template $PEARL_HOME'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
                configfile=pearl_env.config_filename,
            )
        )
        pearl_env.config_filename.parent.mkdir(parents=True, exist_ok=True)
        pearl_conf_template = static / 'templates/pearl.conf.template'
        shutil.copyfile(str(pearl_conf_template), str(pearl_env.config_filename))

    apply(
        "source {home}/boot/sh/pearl.sh".format(
            home=pearl_env.home,
        ),
        "{}/.bashrc".format(os.environ['HOME'])
    )
    messenger.print(
        '{cyan}* {normal}Activated Pearl for Bash'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
        )
    )

    apply(
        "source {home}/boot/sh/pearl.sh".format(
            home=pearl_env.home,
        ),
        "{}/.zshrc".format(os.environ['HOME'])
    )
    messenger.print(
        '{cyan}* {normal}Activated Pearl for Zsh'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
        )
    )

    apply(
        "source {home}/boot/fish/pearl.fish".format(
            home=pearl_env.home,
        ),
        '{}/.config/fish/config.fish'.format(os.environ['HOME'])
    )
    messenger.print(
        '{cyan}* {normal}Activated Pearl for Fish shell'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
        )
    )

    apply(
        "source {home}/boot/vim/pearl.vim".format(
            home=pearl_env.home,
        ),
        "{}/.vimrc".format(os.environ['HOME'])
    )
    messenger.print(
        '{cyan}* {normal}Activated Pearl for Vim editor'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
        )
    )

    apply(
        "(load-file \"{home}/boot/emacs/pearl.el\")".format(home=pearl_env.home),
        "{}/.emacs".format(os.environ['HOME'])
    )
    messenger.print(
        '{cyan}* {normal}Activated Pearl for Emacs editor'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
        )
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
            "Are you sure to REMOVE all the installed packages in {} repository?".format(repo_name),
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
            "source {home}/boot/sh/pearl.sh".format(
                home=pearl_env.home,
            ),
            "{}/.bashrc".format(os.environ['HOME'])
        )
        messenger.print(
            '{cyan}* {normal}Deactivated Pearl for Bash'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
        )

        unapply(
            "source {home}/boot/sh/pearl.sh".format(
                home=pearl_env.home,
            ),
            "{}/.zshrc".format(os.environ['HOME'])
        )
        messenger.print(
            '{cyan}* {normal}Deactivated Pearl for Zsh'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
        )

        unapply(
            "source {home}/boot/fish/pearl.fish".format(
                home=pearl_env.home,
            ),
            '{}/.config/fish/config.fish'.format(os.environ['HOME'])
        )
        messenger.print(
            '{cyan}* {normal}Deactivated Pearl for Fish shell'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
        )

        unapply(
            "source {home}/boot/vim/pearl.vim".format(home=pearl_env.home),
            "{}/.vimrc".format(os.environ['HOME'])
        )
        messenger.print(
            '{cyan}* {normal}Deactivated Pearl for Vim editor'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
        )

        unapply(
            "(load-file \"{home}/boot/emacs/pearl.el\")".format(home=pearl_env.home),
            "{}/.emacs".format(os.environ['HOME'])
        )
        messenger.print(
            '{cyan}* {normal}Deactivated Pearl for Emacs editor'.format(
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
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
