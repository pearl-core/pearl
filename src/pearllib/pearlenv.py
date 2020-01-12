import hashlib
import os
import subprocess

from pathlib import Path

from collections import namedtuple, OrderedDict

from pearllib.messenger import messenger

PearlConf = namedtuple('PearlConf', ['repo_name', 'repos', 'packages'])

_DEFAULT_LOCAL_REPO_NAME = 'local'


class Package:
    def __init__(
            self, pearl_home: Path, repo_name: str,
            name: str, url: str, description: str
    ):
        self._pearl_home = pearl_home
        self._repo_name = repo_name
        self._name = name
        self._url = url
        self._description = description

    @property
    def repo_name(self):
        return self._repo_name

    @property
    def name(self) -> str:
        return self._name

    @property
    def full_name(self) -> str:
        return '{}/{}'.format(self.repo_name, self.name)

    @property
    def url(self) -> str:
        return self._url

    @property
    def description(self) -> str:
        return self._description

    @property
    def dir(self) -> Path:
        return self._pearl_home / 'packages/{}'.format(self.full_name)

    @property
    def vardir(self) -> Path:
        return self._pearl_home / 'var/{}'.format(self.full_name)

    def is_installed(self) -> bool:
        return self.dir.is_dir()

    def is_local(self) -> bool:
        return self.url.startswith('/')

    def __repr__(self):
        return self.full_name

    def __str__(self):
        return self.full_name


class PearlEnvironment:
    def __init__(
            self, home: Path = None,
            config_filename: Path = None, update_repos: bool = False,
            verbose: int = 0, env_initialized: bool = True
    ):
        self._home = self._get_home(home, env_initialized)
        self._config_filename = self._get_config_filename(config_filename, env_initialized)

        if env_initialized:
            self._packages = self._load_packages(update_repos, verbose)

    @property
    def home(self) -> Path:
        return self._home

    @property
    def config_filename(self) -> Path:
        return self._config_filename

    @property
    def packages(self):
        return self._packages

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

    def _load_packages(self, update_repos=False, verbose: int = 0):
        packages = OrderedDict()
        config_filenames = [self.config_filename]
        while config_filenames:
            config_filename = config_filenames.pop(0)
            messenger.debug("Loading Pearl configuration: {}...".format(config_filename))
            pearl_conf = self.load_conf(config_filename)
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
            repo_conf_filenames = self.load_repos(pearl_conf.repos, update_repos, verbose)
            config_filenames.extend(repo_conf_filenames)
        return packages

    def load_conf(self, config_filename: Path):
        local_dict = {}
        exec(config_filename.open().read(), {}, local_dict)
        repo_name = local_dict.get('PEARL_REPO_NAME', _DEFAULT_LOCAL_REPO_NAME)
        packages = OrderedDict()
        for package_name, package_info in local_dict.get('PEARL_PACKAGES', {}).items():
            packages[package_name] = Package(
                self.home,
                repo_name, package_name,
                package_info['url'], package_info.get('description', '')
            )
        return PearlConf(
            repo_name,
            local_dict.get('PEARL_REPOS', ()),
            packages
        )

    def load_repos(self, repos: list, update_repos=False, verbose: int = 0):
        return [self._load_repo(repo, update_repos, verbose) for repo in repos]

    def _load_repo(self, repo: str, update_repos=False, verbose: int = 0):
        m = hashlib.md5()
        # Add \n for compatibility with previous version of Pearl
        m.update('{}\n'.format(repo).encode())
        md5_sum = m.hexdigest()
        if not (self.home / 'repos/{}/.git'.format(md5_sum)).is_dir():
            messenger.info('Initializing {} repository...'.format(repo))
            clone_command = [
                'git', 'clone', '--depth', '1', repo,
                '{}/repos/{}'.format(self.home, md5_sum)
            ]
            if not verbose:
                clone_command.append('--quiet')
            subprocess.run(clone_command)
        elif update_repos:
            messenger.info("Updating {} repository...".format(repo))
            # The option -C works only for git 1.8.5 https://stackoverflow.com/a/20115678
            pull_command = ['git', '-C', '{}/repos/{}'.format(self.home, md5_sum), 'pull']
            if not verbose:
                pull_command.append('--quiet')
            subprocess.run(pull_command)

        return self.home / 'repos/{}/pearl-config/repo.conf'.format(md5_sum)
