import subprocess
from argparse import Namespace
from unittest import mock

from pearllib.pearlenv import PearlEnvironment, Package


class PackageArgs(Namespace):
    def __init__(
            self, no_confirm=False, verbose=0, force=False,
            pattern=".*", package_only=False,
            name="", dest_dir=None,
            packages=()
    ):
        super().__init__()
        self.no_confirm = no_confirm
        self.verbose = verbose
        self.force = force
        self.pattern = pattern
        self.package_only = package_only
        self.name = name
        self.dest_dir = dest_dir
        self.packages = packages


def create_pearl_home(tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    pearl_conf = home_dir / 'pearl.conf'
    pearl_conf.touch()
    return home_dir


def create_pearl_env(home_dir, packages):
    pearl_env = mock.Mock(spec=PearlEnvironment)
    pearl_env.home = home_dir
    pearl_env.packages = packages
    return pearl_env


class PackageTestBuilder:
    def __init__(self, home_dir):
        self.packages = {}
        self.home_dir = home_dir

    def add_local_package(
            self,
            tmp_path, hooks_sh_script,
            repo_name='repo-test',
            package_name='pkg-test',
            is_installed=False,
            depends=(),
    ):
        """Install a package somewhere locally"""
        pkg_dir = tmp_path / '{}/{}'.format(repo_name, package_name)
        (pkg_dir / 'pearl-config').mkdir(parents=True)
        hooks_sh = pkg_dir / 'pearl-config/hooks.sh'
        hooks_sh.touch()
        hooks_sh.write_text(hooks_sh_script)

        if is_installed:
            self._install_package(
                hooks_sh_script,
                repo_name=repo_name,
                package_name=package_name,
            )

        package = Package(
            self.home_dir, repo_name, package_name, str(pkg_dir), '',
            depends=depends
        )
        self._update_packages(package)

    def add_git_package(
            self,
            hooks_sh_script,
            repo_name='repo-test',
            package_name='pkg-test',
            url='https://github.com/pkg',
            git_url=None,
            is_installed=False,
            depends=(),
    ):

        if is_installed:
            if git_url is None:
                git_url = url
            self._install_package(
                hooks_sh_script,
                repo_name='repo-test',
                package_name='pkg-test',
                git_url=git_url
            )
        package = Package(
            self.home_dir, repo_name, package_name, url, '',
            depends=depends
        )
        self._update_packages(package)

    def build(self):
        return self.packages

    def _update_packages(self, package: Package):
        if package.repo_name not in self.packages:
            self.packages[package.repo_name] = {}
        self.packages[package.repo_name][package.name] = package

    def _install_package(
            self,
            hooks_sh_script,
            repo_name='repo-test',
            package_name='pkg-test',
            git_url=None
    ):
        pkg_dir = self.home_dir / 'packages/{}/{}'.format(repo_name, package_name)
        (pkg_dir / 'pearl-config').mkdir(parents=True)

        hooks_sh = pkg_dir / 'pearl-config/hooks.sh'
        hooks_sh.write_text(hooks_sh_script)

        if git_url is not None:
            subprocess.run(
                ['git', '-C', str(pkg_dir), 'init'],
            )
            subprocess.run(
                ['git', '-C', str(pkg_dir), 'remote', 'add', 'origin', git_url],
            )
