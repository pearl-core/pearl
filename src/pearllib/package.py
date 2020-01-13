import re
import shutil
from argparse import Namespace
from pathlib import Path
from textwrap import dedent

import pkg_resources

from pearllib.exceptions import PackageNotInRepoError, PackageAlreadyInstalledError, RepoDoesNotExistError, \
    PackageNotInstalledError, HookFunctionError
from pearllib.messenger import messenger, Color
from pearllib.pearlenv import PearlEnvironment, Package
from pearllib.utils import check_and_copy, ask, run_pearl_bash

_DEFAULT_INPUT = 1000000 * '\n'

# TODO install.sh will be deprecated in upcoming releases:
_HOOK_HEADER_TEMPLATE = dedent("""
PEARL_PKGDIR="{pkgdir}"
PEARL_PKGVARDIR="{vardir}"
PEARL_PKGNAME="{pkgname}"
PEARL_PKGREPONAME="{reponame}"

post_install() {{ :; }}
pre_update() {{ :; }}
post_update() {{ :; }}
pre_remove() {{ :; }}

INSTALL_SH="$PEARL_PKGDIR"/pearl-config/install.sh
[[ -f $INSTALL_SH ]] && source "$INSTALL_SH"

HOOKS_SH="$PEARL_PKGDIR"/pearl-config/hooks.sh
[[ -f $HOOKS_SH ]] && source "$HOOKS_SH"
""")


def _run(
        script: str,
        pearl_env: PearlEnvironment,
        package: Package,
        input: str = None,
        cd_home: bool = False,
        enable_xtrace: bool = False,
        enable_errexit: bool = True,
):
    hookheader = _HOOK_HEADER_TEMPLATE.format(
        pkgdir=package.dir,
        vardir=package.vardir,
        pkgname=package.name,
        reponame=package.repo_name,
    )
    cd = 'cd "$PEARL_HOME"' if cd_home else 'cd "$PEARL_PKGDIR"'
    script = '{hookheader}\n{cd}\n{script}'.format(
        hookheader=hookheader,
        cd=cd,
        script=script,
    )
    run_pearl_bash(script, pearl_env, input=input, enable_xtrace=enable_xtrace, enable_errexit=enable_errexit)


def _lookup_package_full_name(pearl_env: PearlEnvironment, package_full_name: str) -> Package:
    repo_name, short_package_name = package_full_name.split('/')

    if repo_name not in pearl_env.packages:
        raise RepoDoesNotExistError('Skipping {} as {} repository does not exist.'.format(package_full_name, repo_name))
    if short_package_name not in pearl_env.packages[repo_name]:
        raise PackageNotInRepoError('Skipping {} is not in the repositories.'.format(package_full_name))

    return pearl_env.packages[repo_name][short_package_name]


def _lookup_package(pearl_env: PearlEnvironment, package_name: str) -> Package:
    if '/' in package_name:
        return _lookup_package_full_name(pearl_env, package_name)

    for repo_name, repo_packages in pearl_env.packages.items():
        if package_name in repo_packages:
            return repo_packages[package_name]

    raise PackageNotInRepoError('Skipping {} is not in the repositories.'.format(package_name))


def emerge_package(pearl_env: PearlEnvironment, package_name: str, args: Namespace):
    """
    Installs or updates the Pearl package.
    This function is idempotent.
    """
    package = _lookup_package(pearl_env, package_name)
    if package.is_installed():
        update_package(pearl_env, package_name, args=args)
    else:
        install_package(pearl_env, package_name, args=args)


def install_package(pearl_env: PearlEnvironment, package_name: str, args: Namespace):
    """
    Installs the Pearl package.
    """
    package = _lookup_package(pearl_env, package_name)
    if package.is_installed():
        raise PackageAlreadyInstalledError('Skipping {} is already installed.'.format(package))

    messenger.print(
        '{cyan}* {normal}Installing {pkg} package'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            pkg=package,
        )
    )
    package.dir.mkdir(parents=True, exist_ok=True)
    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        quiet = "false" if args.verbose else "true"
        script = dedent(
            """
            install_git_repo {pkgurl} {pkgdir} "" {quiet}
            """
        ).format(pkgdir=package.dir, pkgurl=package.url, quiet=quiet)
        run_pearl_bash(script, pearl_env, input=_DEFAULT_INPUT if args.no_confirm else None)

    package.vardir.mkdir(parents=True, exist_ok=True)

    hook = 'post_install'
    try:
        _run(
            hook, pearl_env, package,
            input=_DEFAULT_INPUT if args.no_confirm else None,
            enable_xtrace=(args.verbose >= 2),
            enable_errexit=(not args.force),
        )
    except Exception as exc:
        msg = "Error while performing {} hook function. Rolling back...".format(hook)
        if args.force:
            messenger.exception("{}: {}".format(msg, exc.args))
        else:
            args.force = True
            remove_package(
                pearl_env, package_name,
                args=args,
            )
            raise HookFunctionError(msg) from exc


def update_package(pearl_env: PearlEnvironment, package_name: str, args: Namespace):
    """
    Updates the Pearl package.
    """
    package = _lookup_package(pearl_env, package_name)
    if not package.is_installed():
        raise PackageNotInstalledError('Skipping {} as it has not been installed.'.format(package))

    messenger.print(
        '{cyan}* {normal}Updating {pkg} package'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            pkg=package,
        )
    )
    if not package.is_local():
        existing_package_url = run_pearl_bash(
            "git -C {} config remote.origin.url".format(package.dir), pearl_env, capture_stdout=True
        ).stdout.strip()
        if existing_package_url != package.url:
            messenger.info("The Git URL for {} has changed from {} to {}".format(
                package.full_name, existing_package_url, package.url
            ))
            if ask(
                "Do you want to replace the package with the new repository?",
                yes_as_default_answer=False
            ):
                remove_package(pearl_env, package_name, args)
                install_package(pearl_env, package_name, args)

    hook = 'pre_update'
    try:
        _run(
            hook, pearl_env, package,
            input=_DEFAULT_INPUT if args.no_confirm else None,
            enable_xtrace=(args.verbose >= 2),
            enable_errexit=(not args.force),
        )
    except Exception as exc:
        msg = "Error while performing {} hook function".format(hook)
        if not args.force:
            raise HookFunctionError(msg) from exc
        messenger.exception("{}: {}".format(msg, exc.args))

    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        quiet = "false" if args.verbose else "true"
        script = dedent(
            """
            update_git_repo {pkgdir} "" {quiet}
            """
        ).format(pkgdir=package.dir, quiet=quiet)
        run_pearl_bash(script, pearl_env, input=_DEFAULT_INPUT if args.no_confirm else None)

    hook = 'post_update'
    try:
        _run(
            hook, pearl_env, package,
            input=_DEFAULT_INPUT if args.no_confirm else None,
            enable_xtrace=(args.verbose >= 2),
            enable_errexit=(not args.force),
        )
    except Exception as exc:
        msg = "Error while performing {} hook function".format(hook)
        if not args.force:
            raise HookFunctionError(msg) from exc
        messenger.exception("{}: {}".format(msg, exc.args))


def remove_package(pearl_env: PearlEnvironment, package_name: str, args: Namespace):
    """
    Remove the Pearl package.
    """
    package = _lookup_package(pearl_env, package_name)
    if not package.is_installed():
        raise PackageNotInstalledError('Skipping {} as it has not been installed.'.format(package))

    messenger.print(
        '{cyan}* {normal}Removing {pkg} package'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            pkg=package,
        )
    )

    hook = 'pre_remove'
    try:
        _run(
            hook, pearl_env, package,
            input=_DEFAULT_INPUT if args.no_confirm else None,
            enable_xtrace=(args.verbose >= 2),
            enable_errexit=(not args.force),
        )
    except Exception as exc:
        msg = "Error while performing {} hook function".format(hook)
        if not args.force:
            raise HookFunctionError(msg) from exc
        messenger.exception("{}: {}".format(msg, exc.args))

    shutil.rmtree(str(package.dir))


def list_packages(pearl_env: PearlEnvironment, args: Namespace):
    """
    Lists or searches Pearl packages.
    """
    uninstalled_packages = []
    installed_packages = []
    pattern = args.pattern if hasattr(args, 'pattern') else ".*"
    regex = re.compile('{}'.format(pattern), flags=re.IGNORECASE)
    for _, repo_packages in pearl_env.packages.items():
        for _, package in repo_packages.items():
            if not regex.search(package.full_name) and not regex.search(package.description):
                continue
            if package.is_installed():
                installed_packages.append(package)
            else:
                uninstalled_packages.append(package)

    for package in uninstalled_packages + installed_packages:
        if args.package_only:
            template = "{reponame}/{package}"
            messenger.print(
                template.format(
                    reponame=package.repo_name,
                    package=package.name,
                )
            )
        else:
            label = "[installed]" if package.is_installed() else ""
            template = "{pink}{{reponame}}/{cyan}{{package}} {{installed}}{normal}".format(
                pink=Color.PINK,
                cyan=Color.CYAN,
                normal=Color.NORMAL,
            )
            messenger.print(
                template.format(
                    reponame=package.repo_name,
                    package=package.name,
                    installed=label,
                )
            )
            messenger.print("    {}".format(package.description))


def create_package(pearl_env: PearlEnvironment, args: Namespace):
    """
    Creates package from template.
    """
    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))
    pearl_config_template = static / 'templates/pearl-config.template'
    dest_pearl_config = args.dest_dir / 'pearl-config'
    if dest_pearl_config.exists():
        raise RuntimeError('The pearl-config directory already exists in {}'.format(args.dest_dir))
    shutil.copytree(str(pearl_config_template), str(dest_pearl_config))

    messenger.info('Updating {} to add package in local repository...'.format(pearl_env.config_filename))
    with pearl_env.config_filename.open('a') as pearl_conf_file:
        pearl_conf_file.write('PEARL_PACKAGES["{}"] = {{"url": "{}"}}\n'.format(args.name, args.dest_dir))

    messenger.info('Run "pearl search local" to see your new local package available')
