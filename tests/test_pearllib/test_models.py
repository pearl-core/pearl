from pathlib import Path

import pytest

from pearllib.models import Package


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


