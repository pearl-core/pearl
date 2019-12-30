import logging
import shutil
import subprocess
from pathlib import Path
from textwrap import dedent

from pearllib.exceptions import PackageNotInRepoError, PackageAlreadyInstalledError, RepoDoesNotExistError, \
    PackageNotInstalledError
from pearllib.pearlenv import PearlEnvironment, Package
from pearllib.utils import check_and_copy, run

logger = logging.getLogger(__name__)


_HOOK_FUNCTIONS_TEMPLATE = dedent("""
set -e -o pipefail

# PATH needs to be updated since GNU Coreutils is required in OSX
# environments. Buava `osx_update_path` cannot be used because in
# order to load osx-compat.sh file the `readlink` command is
# required first (circular dependency).
COREUTILS_GNUBIN="/usr/local/opt/coreutils/libexec/gnubin"
[[ -d "$COREUTILS_GNUBIN" ]] && PATH="$COREUTILS_GNUBIN:$PATH"

PEARL_ROOT={pearlroot}
PEARL_HOME={pearlhome}
PEARL_PKGDIR={pkgdir}
PEARL_PKGVARDIR={vardir}
PEARL_PKGNAME={pkgname}
PEARL_PKGREPONAME={reponame}

cd "$PEARL_HOME"

source "$PEARL_ROOT"/buava/lib/utils.sh
source "$PEARL_ROOT"/buava/lib/osx-compat.sh

post_install() {{ :; }}
pre_update() {{ :; }}
post_update() {{ :; }}
pre_remove() {{ :; }}

INSTALL_SH="$PEARL_PKGDIR"/pearl-config/install.sh
[[ -f $INSTALL_SH ]] && source "$INSTALL_SH"
""")


def _run(script, pearl_env: PearlEnvironment, package: Package, cd_home=False):
    _HOOK_FUNCTIONS_TEMPLATE.format(
        pearlroot=pearl_env.root,
        pearlhome=pearl_env.home,
        pkgdir=package.dir,
        vardir=package.vardir,
        pkgname=package.name,
        reponame=package.repo_name,
    )
    cd = 'cd "$PEARL_HOME"' if cd_home else 'cd "$PEARL_PKGDIR"'
    script = '{hooktemplate}\n{cd}\n{script}'.format(
        hooktemplate=_HOOK_FUNCTIONS_TEMPLATE,
        cd=cd,
        script=script,
    )
    run(script)


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


def emerge_package(pearl_env: PearlEnvironment, package_name: str):
    package = _lookup_package(pearl_env, package_name)
    if package.is_installed():
        update_package(pearl_env, package_name)
    else:
        install_package(pearl_env, package_name)


def install_package(pearl_env: PearlEnvironment, package_name: str):
    # TODO add more tests!
    package = _lookup_package(pearl_env, package_name)
    if package.is_installed():
        raise PackageAlreadyInstalledError('Skipping {} is already installed.'.format(package))

    logger.info("Installing {} package".format(package))
    package.dir.mkdir(parents=True)
    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        script = dedent("""
            source /Users/filippos/pearl/trunk/pearl-python/src/buava/lib/utils.sh
            install_git_repo {pkgurl} {pkgdir}
        """).format(pkgdir=package.dir, pkgurl=package.url)
        run(script)

    package.vardir.mkdir(parents=True)

    _run('post_install', pearl_env, package)


def update_package(pearl_env: PearlEnvironment, package_name: str):
    package = _lookup_package(pearl_env, package_name)
    if not package.is_installed():
        raise PackageNotInstalledError('Skipping {} as it has not been installed.'.format(package))

    logger.info("Updating {} package".format(package))
    # existing_package_url = run("")
    # if not package.is_local() and existing_package_url != package.url:
    #     # TODO handle the case where url has changed
    #     pass

    _run('pre_update', pearl_env, package)

    if package.is_local():
        check_and_copy(Path(package.url), package.dir)
    else:
        script = dedent("""
            source /Users/filippos/pearl/trunk/pearl-python/src/buava/lib/utils.sh
            update_git_repo {pkgdir}
        """).format(pkgdir=package.dir)
        run(script)

    _run('post_update', pearl_env, package)


def remove_package(pearl_env: PearlEnvironment, package_name: str):
    package = _lookup_package(pearl_env, package_name)
    if not package.is_installed():
        raise PackageNotInstalledError('Skipping {} as it has not been installed.'.format(package))

    logger.info("Removing {} package".format(package))

    _run('pre_remove', pearl_env, package)

    shutil.rmtree(str(package.dir))

    _run('post_remove', pearl_env, package, cd_home=True)

