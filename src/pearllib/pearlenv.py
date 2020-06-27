import hashlib
import os
import subprocess

from pathlib import Path

from collections import namedtuple, OrderedDict
from typing import Tuple, Dict, Any

from pearllib.exceptions import PackageNotInRepoError, RepoDoesNotExistError, PackageNotInstalledError
from pearllib.messenger import messenger
from pearllib.models import Package, OS

PearlConf = namedtuple('PearlConf', ['repo_name', 'repos', 'packages'])

_DEFAULT_LOCAL_REPO_NAME = 'local'


class PackageBuilder:
    def __init__(self, pearl_home: Path):
        self.pearl_home = pearl_home

    def build_package(
            self,
            repo_name: str,
            package_name: str,
            packages_info: Dict[str, Dict[str, Dict[str, Any]]],
            packages: Dict[str, Dict[str, Package]]
    ):
        if repo_name in packages and package_name in packages[repo_name]:
            return packages[repo_name][package_name]

        package_info = dict(packages_info[repo_name][package_name])
        os_list = tuple([OS.from_code(os) for os in package_info.get('os', tuple())])

        package_info['os'] = os_list
        depends = package_info.get('depends', tuple())
        if 'depends' in package_info:
            del package_info['depends']
        if repo_name not in packages:
            packages[repo_name] = {}
        packages[repo_name][package_name] = Package(
            self.pearl_home,
            **package_info,
        )
        for package_dep_full_name in depends:
            package_dep_repo_name, package_dep_name = package_dep_full_name.split('/')
            self.build_package(
                package_dep_repo_name,
                package_dep_name,
                packages_info,
                packages
            )
            package_dep = packages[package_dep_repo_name][package_dep_name]
            packages[repo_name][package_name].add_depend(package_dep)

    def build_packages(self, packages_info: Dict[str, Dict[str, Dict[str, Any]]]):
        packages = {}
        for repo_name, repo_packages_info in packages_info.items():
            for package_name, package_info in repo_packages_info.items():
                self.build_package(repo_name, package_name, packages_info, packages)
        return packages


class PackageLoader:
    def __init__(self, home: Path, config_filename: Path):
        self.home = home
        self.config_filename = config_filename

    def load_packages(self, update_repos=False, verbose: int = 0):
        packages = OrderedDict()
        config_filenames = [self.config_filename]
        while config_filenames:
            config_filename = config_filenames.pop(0)
            messenger.debug("Loading Pearl configuration: {}...".format(config_filename))
            pearl_conf = self._load_conf(config_filename)
            packages.update({
                pearl_conf.repo_name: pearl_conf.packages
            })
            messenger.debug(
                "Loaded Pearl configuration: {}. Repo name: {}, number of packages: {}".format(
                    config_filename,
                    pearl_conf.repo_name,
                    len(pearl_conf.packages)
                )
            )
            repo_conf_filenames = self._load_repos(self.home, pearl_conf.repos, update_repos, verbose)
            config_filenames.extend(repo_conf_filenames)
        return packages

    @staticmethod
    def _load_conf(config_filename: Path):
        local_dict = {}
        exec(config_filename.open().read(), {}, local_dict)
        repo_name = local_dict.get('PEARL_REPO_NAME', _DEFAULT_LOCAL_REPO_NAME)
        packages = OrderedDict()
        for package_name, package_info in local_dict.get('PEARL_PACKAGES', {}).items():
            packages_depends_full_name = []
            for package_def_name in package_info.get('depends', tuple()):
                if '/' in package_def_name:
                    packages_depends_full_name.append(package_def_name)
                else:
                    packages_depends_full_name.append("{}/{}".format(repo_name, package_def_name))

            packages[package_name] = dict(
                repo_name=repo_name,
                name=package_name,
                url=package_info['url'],
                description=package_info.get('description'),
                homepage=package_info.get('homepage'),
                author=package_info.get('author'),
                license=package_info.get('license'),
                os=package_info.get('os', tuple()),
                keywords=package_info.get('keywords', tuple()),
                depends=packages_depends_full_name,
            )
        return PearlConf(
            repo_name,
            local_dict.get('PEARL_REPOS', ()),
            packages
        )

    @staticmethod
    def _load_repos(home, repos: list, update_repos=False, verbose: int = 0):
        return [PackageLoader._load_repo(home, repo, update_repos, verbose) for repo in repos]

    @staticmethod
    def _load_repo(home, repo: str, update_repos=False, verbose: int = 0):
        m = hashlib.md5()
        # Add \n for compatibility with previous version of Pearl
        m.update('{}\n'.format(repo).encode())
        md5_sum = m.hexdigest()
        if not (home / 'repos/{}/.git'.format(md5_sum)).is_dir():
            messenger.info('Initializing {} repository...'.format(repo))
            clone_command = [
                'git', 'clone', '--depth', '1', repo,
                '{}/repos/{}'.format(home, md5_sum)
            ]
            if not verbose:
                clone_command.append('--quiet')
            subprocess.run(clone_command)
        elif update_repos:
            messenger.info("Updating {} repository...".format(repo))
            # The option -C works only for git 1.8.5 https://stackoverflow.com/a/20115678
            pull_command = ['git', '-C', '{}/repos/{}'.format(home, md5_sum), 'pull']
            if not verbose:
                pull_command.append('--quiet')
            subprocess.run(pull_command)

        return home / 'repos/{}/pearl-config/repo.conf'.format(md5_sum)


class PearlEnvironment:
    def __init__(
            self, home: Path = None,
            config_filename: Path = None, update_repos: bool = False,
            verbose: int = 0, env_initialized: bool = True
    ):
        self._home = self._get_home(home, env_initialized)
        self._config_filename = self._get_config_filename(config_filename, env_initialized)

        if env_initialized:
            self._packages = self._build_packages(update_repos, verbose)

    @property
    def home(self) -> Path:
        return self._home

    @property
    def config_filename(self) -> Path:
        return self._config_filename

    @property
    def packages(self) -> Dict[str, Dict[str, Package]]:
        return self._packages

    def _lookup_package_full_name(self, package_full_name: str) -> Package:
        repo_name, short_package_name = package_full_name.split('/')

        if repo_name not in self.packages:
            raise RepoDoesNotExistError(
                '{} repository does not exist.'.format(repo_name))
        if short_package_name not in self.packages[repo_name]:
            raise PackageNotInRepoError('{} package is not in any repositories.'.format(package_full_name))

        return self.packages[repo_name][short_package_name]

    def lookup_package(self, package_name: str) -> Package:
        if '/' in package_name:
            return self._lookup_package_full_name(package_name)

        for repo_name, repo_packages in self.packages.items():
            if package_name in repo_packages:
                return repo_packages[package_name]

        raise PackageNotInRepoError('{} package is not in any repositories.'.format(package_name))

    def infer_package(self, package_name: str) -> Package:
        """Builds a package by looking at the file structure."""
        if '/' in package_name:
            repo_name, short_package_name = package_name.split('/')
            package = Package(self.home, repo_name, short_package_name, "None")
            if package.is_installed():
                return package
        else:
            packages_path = self.home / 'packages'
            for repo_path in packages_path.iterdir():
                repo_name = repo_path.name
                package = Package(self.home, repo_name, package_name, "None")
                if package.is_installed():
                    return package
        raise PackageNotInstalledError('{} package has not been installed.'.format(package_name))

    def required_by(self, package: Package) -> Tuple[Package]:
        requires = []
        for _, pkg_info in self.packages.items():
            for _, package_dep in pkg_info.items():
                if package in package_dep.depends:
                    requires.append(package_dep)
        return tuple(requires)

    @staticmethod
    def _get_home(home: Path = None, env_initialized: bool = True) -> Path:
        if home is None:
            xdg_data_home = os.environ.get('XDG_DATA_HOME', '{}/.local/share'.format(os.environ['HOME']))
            default_home = '{}/pearl'.format(xdg_data_home)
            home = Path(default_home)

        messenger.debug("Found Pearl home: {}".format(home))

        if env_initialized:
            if home.exists() and not home.is_dir():
                msg = 'Error: The value in environment variable PEARL_HOME is not a directory: {}.'.format(home)
                messenger.warn(msg)
                raise ValueError(msg)
            elif not home.exists():
                msg = 'Pearl environment has not been initialized. Run "pearl init" first.'
                messenger.warn(msg)
                raise ValueError(msg)

        return home

    @staticmethod
    def _get_config_filename(config_filename: Path = None, env_initialized: bool = True) -> Path:
        if config_filename is None:
            xdg_config_home = os.environ.get('XDG_CONFIG_HOME', '{}/.config'.format(os.environ['HOME']))
            config_home = Path('{}/pearl'.format(xdg_config_home))
            config_filename = config_home / 'pearl.conf'

        if env_initialized:
            if config_filename.exists() and not config_filename.is_file():
                msg = 'Error: The configuration in {} is not a file.'.format(config_filename)
                messenger.warn(msg)
                raise ValueError(msg)
            elif not config_filename.exists():
                msg = 'Pearl configuration file does not exist. Run "pearl init" first.'
                messenger.warn(msg)
                raise ValueError(msg)
        return config_filename

    def _build_packages(self, update_repos=False, verbose: int = 0):
        loader = PackageLoader(self.home, self.config_filename)
        packages_info = loader.load_packages(update_repos, verbose)
        builder = PackageBuilder(self.home)
        return builder.build_packages(packages_info)
