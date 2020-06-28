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
                'Package {} cannot be removed because is required by other packages: {}'.format(
                    package, [str(r) for r in remaining_requires]
                )
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
    script = '{hookheader}\n{cd}\n{script}'.format(
        hookheader=hookheader,
        cd=cd,
        script=script,
    )
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
        dedent("""
        {cyan}Name{normal}: {full_name}
        {cyan}Description{normal}: {description}
        {cyan}Homepage{normal}: {homepage}
        {cyan}URL{normal}: {url}
        {cyan}Author{normal}: {author}
        {cyan}License{normal}: {license}
        {cyan}Operating Systems{normal}: {os}
        {cyan}Keywords{normal}: {keywords}
        {cyan}Installed{normal}: {installed}
        {cyan}Pkg Directory{normal}: {pkg_dir}
        {cyan}Var Directory{normal}: {var_dir}
        {cyan}Depends on{normal}: {depends}
        {cyan}Required by{normal}: {requires}
        """.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            full_name=package.full_name,
            description=package.description,
            homepage=package.homepage,
            url=package.url,
            author=package.author,
            license=package.license,
            os=tuple(o.name.lower() for o in package.os),
            keywords=package.keywords,
            installed=package.is_installed(),
            pkg_dir=package.dir,
            var_dir=package.vardir,
            depends=tuple(d.full_name for d in package.depends),
            requires=tuple(r.full_name for r in requires),
        ))
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
        raise PackageAlreadyInstalledError('{} package is already installed.'.format(package))

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
            message = "{}: {}".format(msg, exc.args)
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
        raise PackageNotInstalledError('{} package has not been installed.'.format(package))

    messenger.print(
        '{cyan}* {normal}Updating {pkg} package'.format(
            cyan=Color.CYAN,
            normal=Color.NORMAL,
            pkg=package,
        )
    )

    if package.has_url_changed():
        messenger.info("The package URL for {} has changed to {}".format(
            package.full_name, package.url
        ))
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
        msg = "Error while performing {} hook function".format(hook)
        if not args.force:
            raise HookFunctionError(msg) from exc
        message = "{}: {}".format(msg, exc.args)
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)

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
        message = "{}: {}".format(msg, exc.args)
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)


def remove_package(pearl_env: PearlEnvironment, package: Package, args: Namespace):
    """
    Remove the Pearl package.
    """
    if not package.is_installed():
        raise PackageNotInstalledError('{} package has not been installed.'.format(package))

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
        message = "{}: {}".format(msg, exc.args)
        if args.verbose:
            messenger.exception(message)
        else:
            messenger.error(message)

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
            if not regex.search(package.full_name) \
                    and not regex.search(package.description) \
                    and all([regex.search(key) is None for key in package.keywords]):
                continue
            if package.is_installed():
                installed_packages.append(package)
            else:
                uninstalled_packages.append(package)

    total_packages = uninstalled_packages + installed_packages
    for package in total_packages:
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
    return total_packages


def create_package(pearl_env: PearlEnvironment, args: Namespace):
    """
    Creates package from template.
    """
    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))
    pearl_config_template = static / 'templates/pearl-config.template'
    dest_pearl_config = args.dest_dir / 'pearl-config'
    if dest_pearl_config.exists():
        raise PackageCreateError('The pearl-config directory already exists in {}'.format(args.dest_dir))
    shutil.copytree(str(pearl_config_template), str(dest_pearl_config))

    messenger.info('Updating {} to add package in local repository...'.format(pearl_env.config_filename))
    with pearl_env.config_filename.open('a') as pearl_conf_file:
        pearl_conf_file.write('PEARL_PACKAGES["{}"] = {{"url": "{}"}}\n'.format(args.name, args.dest_dir))

    messenger.info('Run "pearl search local" to see your new local package available')
