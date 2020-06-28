from enum import Enum
from pathlib import Path
from typing import Tuple

from pearllib.utils import run_bash


class OS(Enum):
    LINUX = 'linux'
    OSX = 'osx'

    @staticmethod
    def from_code(code: str) -> 'OS':
        return OS.__members__[code.upper()]

    def __str__(self):
        return self.name


class Package:
    def __init__(
            self, pearl_home: Path, repo_name: str,
            name: str, url: str,
            description: str = None,
            homepage: str = None,
            author: str = None,
            license: str = None,
            os: tuple = None,
            keywords: tuple = None,
            depends: tuple = None,
    ):
        self._pearl_home = pearl_home

        self._repo_name = repo_name
        if not repo_name:
            raise ValueError("repository name is mandatory field")
        self._name = name
        if not name:
            raise ValueError("package name is mandatory field")
        self._url = url
        if not url:
            raise ValueError("package url is mandatory field")

        self._description = description or str(None)
        self._homepage = homepage or str(None)
        self._author = author or str(None)
        self._license = license or str(None)

        self._os = os or tuple()
        self._keywords = keywords or tuple()
        self._depends = depends or tuple()

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
    def homepage(self) -> str:
        return self._homepage

    @property
    def author(self) -> str:
        return self._author

    @property
    def license(self) -> str:
        return self._license

    @property
    def os(self) -> Tuple[OS]:
        return self._os

    @property
    def keywords(self) -> Tuple[str]:
        return self._keywords

    @property
    def depends(self) -> Tuple['Package']:
        return self._depends

    @property
    def dir(self) -> Path:
        return self._pearl_home / 'packages/{}'.format(self.full_name)

    @property
    def vardir(self) -> Path:
        return self._pearl_home / 'var/{}'.format(self.full_name)

    def add_depend(self, package: 'Package'):
        self._depends = self._depends + (package,)

    def is_installed(self) -> bool:
        return self.dir.is_dir()

    @property
    def _dir_url(self) -> str:
        """
        This function allows to understand whether a package URL has changed over the time. There are two URLs:
        - The "package URL" - is loaded by looking at the pearl configuration file (pearl.conf)
        - The "package directory URL" - is retrieved by looking at the package directory
          content using "git config" command.

        Returns
        -------
        If the package is not local (a git repo), the function return the "package directory URL"
        using "git config" command.

        Raises
        ------
        If package is not a git repo the git command will fail.
        """
        # This information is always computed given that it can change over the time
        # (i.e. when replacing URL repo from local to git and vice versa)
        package_dir_url = run_bash(
            "git -C {} config remote.origin.url".format(self.dir),
            check=False, capture_stdout=True
        ).stdout.strip()
        return package_dir_url

    def has_url_changed(self) -> bool:
        if self.is_local():
            # If package source comes from a local directory assume that the url did not change
            return False
        if not self._is_dir_git_repo():
            # If the existing package dir is not a git repo and the package source is not local, it must have changed
            return True
        return self._dir_url != self.url

    def _is_dir_git_repo(self) -> bool:
        # This information is always computed given that it can change over the time
        # (i.e. when replacing URL repo from local to git and vice versa)
        return run_bash(
            "git -C {} rev-parse --is-inside-work-tree".format(self.dir),
            capture_stdout=True, capture_stderr=True, check=False
        ).stdout.strip() == "true"

    def is_local(self) -> bool:
        return self.url.startswith('/')

    def __repr__(self):
        return "Package({})".format(self.full_name)

    def __str__(self):
        return self.full_name

    def __hash__(self):
        return hash(self.full_name)

    def __eq__(self, other: 'Package'):
        return self.full_name == other.full_name
