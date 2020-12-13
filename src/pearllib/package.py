import re
import shutil
from argparse import Namespace
from pathlib import Path
from textwrap import dedent
from typing import Sequence, Set

import pkg_resources

from pearllib.exceptions import PackageAlreadyInstalledError, \
    PackageNotInstalledError, HookFunctionError, PackageRequiredByOtherError, PackageCreateError
from pearllib.messenger import messenger, Color
from pearllib.pearlenv import PearlEnvironment, Package
from pearllib.utils import check_and_copy, OrderedSet
from pearllib.utils import run_pearl_bash

_DEFAULT_INPUT = 1000000 * '\n'

_HOOK_HEADER_TEMPLATE = dedent("""
PEARL_PKGDIR="{pkgdir}"
PEARL_PKGVARDIR="{vardir}"
PEARL_PKGNAME="{pkgname}"
PEARL_PKGREPONAME="{reponame}"

post_install() {{ :; }}
pre_update() {{ :; }}
post_update() {{ :; }}
pre_remove() {{ :; }}

HOOKS_SH="$PEARL_PKGDIR"/pearl-config/hooks.sh
[[ -f $HOOKS_SH ]] && source "$HOOKS_SH"
""")


def install_packages(
        pearl_env: PearlEnvironment,
        args: Namespace
):
    package_list = closure_dependency_tree(args.packages)
    # Perform emerge command for all dependencies for the specified packages
    for package in package_list:
        if package in args.packages:
            install_package(pearl_env, package, args)
        else:
            emerge_package(pearl_env, package, args)


def update_packages(
        pearl_env: PearlEnvironment,
        args: Namespace
):
    package_list = closure_dependency_tree(args.packages)
    # Perform emerge command for all dependencies for the specified packages
    for package in package_list:
        if package in args.packages:
            update_package(pearl_env, package, args)
        else:
            emerge_package(pearl_env, package, args)


def emerge_packages(
        pearl_env: PearlEnvironment,
        args: Namespace
):
    package_list = closure_dependency_tree(args.packages)
    for package in package_list:
        emerge_package(pearl_env, package, args)


def remove_packages(
        pearl_env: PearlEnvironment,
        args: Namespace
):
    package_list = list(closure_dependency_tree(args.packages))
    package_list.reverse()
    for package in package_list:
        if package not in args.packages:
            continue
        requires = [r for r in pearl_env.required_by(package) if r.is_installed()]
        remaining_requires = set(requires).difference(args.packages)
        if remaining_requires:
            raise PackageRequiredByOtherError(
                f'Package {package} cannot be removed because is required by other packages: {[str(r) for r in remaining_requires]}'
            )
        remove_package(pearl_env, package, args)


def info_packages(
        pearl_env: PearlEnvironment,
        args: Namespace
):
    for package in args.packages:
        info_package(pearl_env, package, args)


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
    script = f'{hookheader}\n{cd}\n{script}'
    run_pearl_bash(script, pearl_env, input=input, enable_xtrace=enable_xtrace, enable_errexit=enable_errexit)


def closure_dependency_tree(
        packages: Sequence[Package],
) -> Sequence:
    visited = set()
    return list(_closure_dependency_tree(OrderedSet(packages), visited))


def _closure_dependency_tree(
        packages: OrderedSet,
        visited: Set,
) -> OrderedSet:
    accumulator = OrderedSet()
    for package in packages:
        visited.add(package)
        accumulator.update(
            _closure_dependency_tree(
                OrderedSet([d for d in package.depends if d not in visited]),
                visited
            )
        )
        accumulator.add(package)
    return accumulator


def info_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Provide info about a package.
    """
    requires = pearl_env.required_by(package)
    messenger.print(
        dedent(
            f"""
            {Color.CYAN}Name{Color.NORMAL}: {package.full_name}
            {Color.CYAN}Description{Color.NORMAL}: {package.description}
            {Color.CYAN}Homepage{Color.NORMAL}: {package.homepage}
            {Color.CYAN}URL{Color.NORMAL}: {package.url}
            {Color.CYAN}Author{Color.NORMAL}: {package.author}
            {Color.CYAN}License{Color.NORMAL}: {package.license}
            {Color.CYAN}Operating Systems{Color.NORMAL}: {tuple(o.name.lower() for o in package.os)}
            {Color.CYAN}Keywords{Color.NORMAL}: {package.keywords}
            {Color.CYAN}Installed{Color.NORMAL}: {package.is_installed()}
            {Color.CYAN}Pkg Directory{Color.NORMAL}: {package.dir}
            {Color.CYAN}Var Directory{Color.NORMAL}: {package.vardir}
            {Color.CYAN}Depends on{Color.NORMAL}: {tuple(d.full_name for d in package.depends)}
            {Color.CYAN}Required by{Color.NORMAL}: {tuple(r.full_name for r in requires)}
            """
        )
    )
    return package, requires


def emerge_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Installs or updates the Pearl package.
    This function is idempotent.
    """
    if package.is_installed():
        update_package(pearl_env, package, args=args)
    else:
        install_package(pearl_env, package, args=args)


def install_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Installs the Pearl package.
    """
    if package.is_installed():
        raise PackageAlreadyInstalledError(f'{package} package is already installed.')

    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Installing {package} package'
    )
    package.dir.mkdir(parents=True, exist_ok=True)
    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        quiet = "false" if args.verbose else "true"
        script = dedent(
            f"""
            install_git_repo {package.url} {package.dir} "" {quiet}
            """
        )
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
        msg = f"Error while performing {hook} hook function. Rolling back..."
        if args.force:
            message = f"{msg}: {exc.args}"
            if args.verbose:
                messenger.exception(message)
            else:
                messenger.error(message)
        else:
            args.force = True
            remove_package(
                pearl_env, package,
                args=args,
            )
            raise HookFunctionError(msg) from exc


def update_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Updates the Pearl package.
    """
    if not package.is_installed():
        raise PackageNotInstalledError(f'{package} package has not been installed.')

    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Updating {package} package'
    )

    if package.has_url_changed():
        messenger.info(f"The package URL for {package.full_name} has changed to {package.url}")
        messenger.info("Replacing the package with the new repository...")
        remove_package(pearl_env, package, args)
        install_package(pearl_env, package, args)

    hook = 'pre_update'
    try:
        _run(
            hook, pearl_env, package,
            input=_DEFAULT_INPUT if args.no_confirm else None,
            enable_xtrace=(args.verbose >= 2),
            enable_errexit=(not args.force),
        )
    except Exception as exc:
        msg = f"Error while performing {hook} hook function"
        if not args.force:
            raise HookFunctionError(msg) from exc
        message = f"{msg}: {exc.args}"
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)

    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        quiet = "false" if args.verbose else "true"
        script = dedent(
            f"""
            update_git_repo {package.dir} "" {quiet}
            """
        )
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
        msg = f"Error while performing {hook} hook function"
        if not args.force:
            raise HookFunctionError(msg) from exc
        message = f"{msg}: {exc.args}"
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)


def remove_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Remove the Pearl package.
    """
    if not package.is_installed():
        raise PackageNotInstalledError(f'{package} package has not been installed.')

    messenger.print(
        f'{Color.CYAN}* {Color.NORMAL}Removing {package} package'
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
        msg = f"Error while performing {hook} hook function"
        if not args.force:
            raise HookFunctionError(msg) from exc
        message = f"{msg}: {exc.args}"
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)

    shutil.rmtree(str(package.dir))


def _list_packages(pearl_env: PearlEnvironment, args: Namespace):
    uninstalled_packages = []
    installed_packages = []
    pattern = args.pattern if hasattr(args, 'pattern') else ".*"
    regex = re.compile(f'{pattern}', flags=re.IGNORECASE)
    for _, repo_packages in pearl_env.packages.items():
        for _, package in repo_packages.items():
            if not regex.search(package.full_name) \
                    and not regex.search(package.description) \
                    and all([regex.search(key) is None for key in package.keywords]):
                continue
            if package.is_installed():
                installed_packages.append(package)
            else:
                uninstalled_packages.append(package)
    if args.installed_only:
        return installed_packages
    return uninstalled_packages + installed_packages


def list_packages(pearl_env: PearlEnvironment, args: Namespace):
    """
    Lists or searches Pearl packages.
    """
    total_packages = _list_packages(pearl_env, args)
    if args.dependency_tree:
        total_packages = list(closure_dependency_tree(total_packages))

    for package in total_packages:
        if args.package_only:
            messenger.print(
                f"{package.repo_name}/{package.name}"
            )
        else:
            label = "[installed]" if package.is_installed() else ""
            messenger.print(
                f"{Color.PINK}{package.repo_name}/{Color.CYAN}{package.name} {label}{Color.NORMAL}"
            )
            messenger.print(f"    {package.description}")
    return total_packages


def create_package(pearl_env: PearlEnvironment, args: Namespace):
    """
    Creates package from template.
    """
    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))
    pearl_config_template = static / 'templates/pearl-config.template'
    dest_pearl_config = args.dest_dir / 'pearl-config'
    if dest_pearl_config.exists():
        raise PackageCreateError(f'The pearl-config directory already exists in {args.dest_dir}')
    shutil.copytree(str(pearl_config_template), str(dest_pearl_config))

    messenger.info(f'Updating {pearl_env.config_filename} to add package in local repository...')
    with pearl_env.config_filename.open('a') as pearl_conf_file:
        pearl_conf_file.write(f'PEARL_PACKAGES["{args.name}"] = {{"url": "{args.dest_dir}"}}\n')

    messenger.info('Run "pearl search local" to see your new local package available')
