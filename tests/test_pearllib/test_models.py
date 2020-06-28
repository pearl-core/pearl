from collections import namedtuple
from pathlib import Path
from unittest import mock

import pytest

from pearllib.models import Package

_MODULE_UNDER_TEST = 'pearllib.models'


def test_package():
    pkg = Package(Path('/home'), 'repo', 'pkg1', 'https://pkg1', 'pkg1 descr')
    assert pkg.name == 'pkg1'
    assert pkg.repo_name == 'repo'
    assert pkg.full_name == 'repo/pkg1'
    assert pkg.url == 'https://pkg1'
    assert pkg.description == 'pkg1 descr'
    assert pkg.dir == Path('/home/packages/repo/pkg1')
    assert pkg.vardir == Path('/home/var/repo/pkg1')
    assert not pkg.is_local()

    pkg = Package(Path('/home'), 'repo', 'pkg1', '/pkg1', 'pkg1 descr')
    assert pkg.is_local()


def test_package_default_values():
    pkg = Package(Path('/home'), 'repo', 'pkg', 'https://pkg.com')
    assert pkg.description == "None"
    assert pkg.homepage == "None"
    assert pkg.author == "None"
    assert pkg.license == "None"
    assert pkg.os == ()
    assert pkg.depends == ()
    assert pkg.keywords == ()


def test_package_raise_missing_values():
    with pytest.raises(ValueError):
        Package(Path('/home'), None, 'pkg', 'url')
    with pytest.raises(ValueError):
        Package(Path('/home'), 'repo', None, 'url')
    with pytest.raises(ValueError):
        Package(Path('/home'), 'repo', 'pkg', None)


@pytest.mark.parametrize(
    'git_url, package_url, expected_result',
    [
        # Git URL is the same
        pytest.param(
            'https://pkg1',
            'https://pkg1',
            False,
        ),
        # From local to git
        pytest.param(
            None,
            'https://pkg1',
            True,
        ),
        # From git to local
        pytest.param(
            'https://pkg1',
            '/my/local/path',
            False,
        ),
        # From local to local
        pytest.param(
            None,
            '/my/local/path',
            False,
        ),
        # From Git to another git URL
        pytest.param(
            'https://pkg1',
            'https://pkg2',
            True,
        ),
    ]
)
def test_package_has_url_changed(git_url, package_url, expected_result):
    is_dir_local = True if git_url is None else False
    OutProces = namedtuple('OutProcess', ['stdout'])

    def _run_bash(command, **kwargs):
        if 'config' in command:
            return OutProces(git_url)
        elif 'rev-parse' in command:
            return OutProces('false') if is_dir_local else OutProces('true')

    pkg = Package(Path('/home'), 'repo', 'pkg1', package_url, 'pkg1 descr')
    with mock.patch(_MODULE_UNDER_TEST + ".run_bash") as run_mock:
        run_mock.side_effect = _run_bash
        assert pkg.has_url_changed() == expected_result
