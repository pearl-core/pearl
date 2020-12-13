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
            messenger.debug(f"Loading Pearl configuration: {config_filename}...")
            pearl_conf = self._load_conf(config_filename)
            packages.update({
                pearl_conf.repo_name: pearl_conf.packages
            })
            messenger.debug(
                f"Loaded Pearl configuration: {config_filename}. Repo name: {pearl_conf.repo_name}, number of packages: {len(pearl_conf.packages)}"
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
                    packages_depends_full_name.append(f"{repo_name}/{package_def_name}")

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
        m.update(f'{repo}\n'.encode())
        md5_sum = m.hexdigest()
        if not (home / f'repos/{md5_sum}/.git').is_dir():
            messenger.info(f'Initializing {repo} repository...')
            clone_command = [
                'git', 'clone', '--depth', '1', repo,
                f'{home}/repos/{md5_sum}'
            ]
            if not verbose:
                clone_command.append('--quiet')
            subprocess.run(clone_command)
        elif update_repos:
            messenger.info(f"Updating {repo} repository...")
            # The option -C works only for git 1.8.5 https://stackoverflow.com/a/20115678
            pull_command = ['git', '-C', f'{home}/repos/{md5_sum}', 'pull']
            if not verbose:
                pull_command.append('--quiet')
            subprocess.run(pull_command)

        return home / f'repos/{md5_sum}/pearl-config/repo.conf'


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
                f'{repo_name} repository does not exist.')
        if short_package_name not in self.packages[repo_name]:
            raise PackageNotInRepoError(f'{package_full_name} package is not in any repositories.')

        return self.packages[repo_name][short_package_name]

    def lookup_package(self, package_name: str) -> Package:
        if '/' in package_name:
            return self._lookup_package_full_name(package_name)

        for repo_name, repo_packages in self.packages.items():
            if package_name in repo_packages:
                return repo_packages[package_name]

        raise PackageNotInRepoError(f'{package_name} package is not in any repositories.')

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
        raise PackageNotInstalledError(f'{package_name} package has not been installed.')

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
            xdg_data_home = os.environ.get('XDG_DATA_HOME', f"{os.environ['HOME']}/.local/share")
            default_home = f'{xdg_data_home}/pearl'
            home = Path(default_home)

        messenger.debug(f"Found Pearl home: {home}")

        if env_initialized:
            if home.exists() and not home.is_dir():
                msg = f'Error: The value in environment variable PEARL_HOME is not a directory: {home}.'
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
            xdg_config_home = os.environ.get('XDG_CONFIG_HOME', f"{os.environ['HOME']}/.config")
            config_home = Path(f'{xdg_config_home}/pearl')
            config_filename = config_home / 'pearl.conf'

        if env_initialized:
            if config_filename.exists() and not config_filename.is_file():
                msg = f'Error: The configuration in {config_filename} is not a file.'
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
