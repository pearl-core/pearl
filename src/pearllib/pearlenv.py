import hashlib
import os
import subprocess

from pathlib import Path

from collections import namedtuple, OrderedDict

from pearllib.messenger import messenger

PearlConf = namedtuple('PearlConf', ['repo_name', 'repos', 'packages'])

PearlOptions = namedtuple('PearlOptions', ['no_confirm', 'verbose', 'force'])
PearlOptions.__new__.__defaults__ = (False, 0, False)


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
            self, home: Path = None, root: Path = None,
            config_filename: Path = None, update_repos: bool = False,
            verbose: int = 0,
    ):
        self._home = self._get_home(home, verbose)
        self._root = self._get_root(root, verbose)

        self.config_filename = self._get_config_filename(self._home, config_filename)

        self._packages = self._load_packages(update_repos, verbose)

    @property
    def home(self):
        return self._home

    @property
    def root(self):
        return self._root

    @property
    def packages(self):
        return self._packages

    @staticmethod
    def _get_home(home: Path = None, verbose: int = 0):
        if home is None:
            default_home = '{}/.config/pearl'.format(os.environ['HOME'])
            home = Path(os.environ.get('PEARL_HOME', default_home))

        if verbose:
            messenger.info("Found Pearl home: {}".format(home))

        if not home.is_dir():
            msg = 'Error: The value in environment variable PEARL_HOME is not a directory: {}'.format(home)
            messenger.warn(msg)
            raise ValueError(msg)

        return home

    @staticmethod
    def _get_root(root: Path = None, verbose: int = 0):
        if root is None:
            root = Path(os.environ['PEARL_ROOT'])

        if verbose:
            messenger.info("Found Pearl root: {}".format(root))

        if not root.is_dir():
            msg = 'Error: The value in environment variable PEARL_ROOT is not a directory: {}'.format(root)
            messenger.warn(msg)
            raise ValueError(msg)

        return root

    @staticmethod
    def _get_config_filename(home: Path, config_filename: Path):
        if config_filename is None:
            return home / 'pearl.conf'
        return config_filename

    def _load_packages(self, update_repos=False, verbose: int = 0):
        packages = OrderedDict()
        config_filenames = [self.config_filename]
        while config_filenames:
            config_filename = config_filenames.pop(0)
            if verbose:
                messenger.info("Loading Pearl configuration: {}...".format(config_filename))
            pearl_conf = self.load_conf(config_filename)
            packages.update({
                pearl_conf.repo_name: pearl_conf.packages
            })
            if verbose:
                messenger.info(
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
        repo_name = local_dict.get('PEARL_REPO_NAME', 'default')
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

