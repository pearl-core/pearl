import os
from pathlib import Path

import pytest

from unittest import mock

from pearllib.exceptions import RepoDoesNotExistError, PackageNotInRepoError, PackageNotInstalledError
from pearllib.pearlenv import PearlEnvironment, PackageBuilder, PackageLoader
from pearllib.models import Package
from test_pearllib.utils import create_pearl_home

_MODULE_UNDER_TEST = 'pearllib.pearlenv'


def pkg_info(
        repo_name='repo1', name='pkg1',
        url='url1', description='descr', homepage='http://pkg1',
        author='auth1', license='GPL', os=('linux',),
        keywords=('kw1',), depends=()
):
    return dict(
        repo_name=repo_name,
        name=name,
        url=url,
        description=description,
        homepage=homepage,
        author=author,
        license=license,
        os=os,
        keywords=keywords,
        depends=depends,
    )


@pytest.mark.parametrize(
    'packages_info',
    [
        pytest.param({}),
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info()
                }
            },
        ),
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info(depends=('repo1/pkg2',)),
                    'pkg2': pkg_info(name='pkg2'),
                },
            },
        ),
        # Self cycle
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info(depends=('repo1/pkg1',)),
                },
            },
        ),
        # Cycle
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info(depends=('repo1/pkg2',)),
                    'pkg2': pkg_info(name='pkg2', depends=('repo1/pkg1',)),
                },
            },
        ),
        # Common dep
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info(depends=('repo1/pkg3',)),
                    'pkg2': pkg_info(name='pkg2', depends=('repo1/pkg3',)),
                    'pkg3': pkg_info(name='pkg3'),
                },
            },
        ),
        # Different repos
        pytest.param(
            {
                'repo1': {
                    'pkg1': pkg_info(name='pkg1', depends=('repo2/pkg2',)),
                    'pkg3': pkg_info(name='pkg3', depends=()),
                },
                'repo2': {
                    'pkg2': pkg_info(name='pkg2', repo_name='repo2', depends=('repo1/pkg3',)),
                }
            },
        ),
    ]
)
def test_package_builder(tmp_path, packages_info):
    builder = PackageBuilder(tmp_path)
    actual_packages = builder.build_packages(packages_info)
    for repo_name in packages_info.keys():
        for pkg_name in packages_info[repo_name].keys():
            actual_package = actual_packages[repo_name][pkg_name]
            package_info = packages_info[repo_name][pkg_name]
            assert package_info['name'] == actual_package.name
            assert package_info['repo_name'] == actual_package.repo_name
            assert '{}/{}'.format(package_info['repo_name'], package_info['name']) == actual_package.full_name
            assert package_info['url'] == actual_package.url
            assert package_info['description'] == actual_package.description
            assert package_info['homepage'] == actual_package.homepage
            assert package_info['author'] == actual_package.author
            assert package_info['license'] == actual_package.license
            assert package_info['os'] == tuple([o.name.lower() for o in actual_package.os])
            assert package_info['keywords'] == actual_package.keywords
            for pkg_dep_name in package_info['depends']:
                assert pkg_dep_name in [dep.full_name for dep in actual_package.depends]

            assert str(actual_package.dir) == str(tmp_path / 'packages/{}'.format(actual_package.full_name))
            assert str(actual_package.vardir) == str(tmp_path / 'var/{}'.format(actual_package.full_name))


@pytest.mark.parametrize(
    'home, expected_home',
    [
        pytest.param(
            None,
            'home-dir/.local/share/pearl'
        ),
        pytest.param(
            'home-dir/.local/share/pearl',
            'home-dir/.local/share/pearl'
        ),
        pytest.param(
            'home/myhome3/.local/share/pearl3',
            'home/myhome3/.local/share/pearl3',
        ),
    ]
)
def test_pearl_env_home(home, expected_home, tmp_path):
    with mock.patch(_MODULE_UNDER_TEST + '.os') as os_mock:
        home_dir = tmp_path / 'home-dir'
        (home_dir / '.local/share/pearl').mkdir(parents=True)
        default_environ = {'HOME': home_dir}

        os_mock.environ = dict(default_environ)

        if home is not None:
            home = tmp_path / home
            home.mkdir(parents=True, exist_ok=True)
        assert str(PearlEnvironment._get_home(home=home)) == str(tmp_path / expected_home)


def test_pearl_env_home_not_exist():
    with mock.patch(_MODULE_UNDER_TEST + '.os') as os_mock:
        os_mock.environ = {'HOME': '/home/myhome'}
        PearlEnvironment._get_home(env_initialized=False)
        with pytest.raises(ValueError):
            PearlEnvironment._get_home()


def test_pearl_env_home_not_directory(tmp_path):
    (tmp_path / 'file').touch()
    with pytest.raises(ValueError):
        PearlEnvironment._get_home(tmp_path / 'file')


def test_config_filename():
    assert PearlEnvironment._get_config_filename(Path('/myhome2/pearl.conf'), False) == Path('/myhome2/pearl.conf')
    assert PearlEnvironment._get_config_filename(None, False) == Path('{}/.config/pearl/pearl.conf'.format(os.environ['HOME']))


def test_config_filename_does_not_exist():
    with pytest.raises(ValueError):
        PearlEnvironment._get_config_filename(Path('/myhome2/pearl.conf'), True)


def test_config_filename_not_a_file(tmp_path):
    with pytest.raises(ValueError):
        PearlEnvironment._get_config_filename(tmp_path, True)


def test_packages(tmp_path):
    pearl_home = tmp_path / 'home'
    pearl_home.mkdir()

    pearl_conf = pearl_home / 'pearl.conf'
    pearl_conf.write_text("""
PEARL_REPO_NAME = 'test'
PEARL_PACKAGES = {
    'b-test-pkg': {
        'url': 'https://github.com/username/test-pkg',
        'description': 'My descr'
    },
    'a-test-pkg': {
        'url': 'https://github.com/username/test-pkg',
        'description': 'My descr'
    },
}
""")
    pearl_env = PearlEnvironment(home=pearl_home, config_filename=pearl_conf)
    assert pearl_env.packages['test']['a-test-pkg'].url == 'https://github.com/username/test-pkg'
    assert pearl_env.packages['test']['a-test-pkg'].description == 'My descr'
    assert pearl_env.packages['test']['b-test-pkg'].url == 'https://github.com/username/test-pkg'
    assert pearl_env.packages['test']['b-test-pkg'].description == 'My descr'


def test_load_repos_init_repo(tmp_path):
    pearl_home = tmp_path / 'home'
    pearl_home.mkdir()

    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess_mock:
        repo_path = pearl_home / 'repos/a10c24fd1961ce3d1b87c05cc008b593'
        assert PackageLoader._load_repos(pearl_home, ('https://github.com/pearl-hub/repo.git',)) == [repo_path / 'pearl-config/repo.conf']

        assert subprocess_mock.run.call_args[0][0] == ['git', 'clone', '--depth', '1', 'https://github.com/pearl-hub/repo.git', str(repo_path), '--quiet']


@pytest.mark.parametrize(
    'update_repos',
    [
        pytest.param(True),
        pytest.param(False)
    ]
)
def test_load_repos_update_repo(update_repos, tmp_path):
    pearl_home = tmp_path / 'home'
    pearl_home.mkdir()

    repo_dir = pearl_home / "repos/a10c24fd1961ce3d1b87c05cc008b593/.git"
    repo_dir.mkdir(parents=True)

    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess_mock:
        repo_path = pearl_home / 'repos/a10c24fd1961ce3d1b87c05cc008b593'
        assert PackageLoader._load_repos(pearl_home, ('https://github.com/pearl-hub/repo.git',), update_repos=update_repos) == [repo_path / 'pearl-config/repo.conf']
        if update_repos:
            assert subprocess_mock.run.call_args[0][0] == ['git', '-C', str(repo_path), 'pull', '--quiet']
        else:
            assert subprocess_mock.run.call_count == 0


def test_lookup_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg-test": {
                "repo_name": "repo-test",
                "name": "pkg-test",
                "url": "/blah",
                "depends": ["repo-test/pkg-test2"]
            },
            "pkg-test2": {
                "repo_name": "repo-test",
                "name": "pkg-test2",
                "url": "/blah",
                "depends": []
            }
        }
    }
    builder = PackageBuilder(home_dir)
    packages = builder.build_packages(packages_info)
    pearl_env = PearlEnvironment(
        home_dir, env_initialized=False
    )
    pearl_env._packages = packages
    actual_package = pearl_env.lookup_package('repo-test/pkg-test')
    assert actual_package.full_name == 'repo-test/pkg-test'

    actual_package = pearl_env.lookup_package('pkg-test')
    assert actual_package.full_name == 'repo-test/pkg-test'


def test_lookup_package_repo_no_exists(tmp_path):
    home_dir = (tmp_path / 'home')
    home_dir.mkdir(parents=True)
    (tmp_path / 'config').mkdir(parents=True)
    conffile = (tmp_path / 'config/pearl.conf')
    conffile.touch()
    with pytest.raises(RepoDoesNotExistError):
        pearl_env = PearlEnvironment(home_dir, conffile)
        pearl_env.lookup_package('test/pkg-test')


def test_lookup_package_not_in_repo(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    packages_info = {
        "repo-test": {
            "pkg-test": {
                "repo_name": "repo-test",
                "name": "pkg-test",
                "url": "/blah",
            },
        }
    }
    builder = PackageBuilder(home_dir)
    packages = builder.build_packages(packages_info)

    pearl_env = PearlEnvironment(
        home_dir, env_initialized=False
    )
    pearl_env._packages = packages
    with pytest.raises(PackageNotInRepoError):
        pearl_env.lookup_package('repo-test/pkg-a-test')

    with pytest.raises(PackageNotInRepoError):
        pearl_env.lookup_package('pkg-a-test')


def test_infer_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'packages').mkdir(parents=True)
    pearl_env = PearlEnvironment(
        home_dir, env_initialized=False
    )

    with pytest.raises(PackageNotInstalledError):
        pearl_env.infer_package('repo-test/pkg-test')
    with pytest.raises(PackageNotInstalledError):
        pearl_env.infer_package('pkg-test')

    (home_dir / 'packages/repo-test/pkg-test').mkdir(parents=True)

    actual_package = pearl_env.infer_package('repo-test/pkg-test')
    assert actual_package.full_name == 'repo-test/pkg-test'

    actual_package = pearl_env.infer_package('pkg-test')
    assert actual_package.full_name == 'repo-test/pkg-test'


def test_required_by(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg-test": {
                "repo_name": "repo-test",
                "name": "pkg-test",
                "url": "/blah",
                "depends": ["repo-test/pkg-test2"]
            },
            "pkg-test2": {
                "repo_name": "repo-test",
                "name": "pkg-test2",
                "url": "/blah",
                "depends": []
            }
        }
    }
    builder = PackageBuilder(home_dir)
    packages = builder.build_packages(packages_info)
    pearl_env = PearlEnvironment(
        home_dir, env_initialized=False
    )
    pearl_env._packages = packages
    requires = pearl_env.required_by(
        Package(home_dir, "repo-test", "pkg-test2", "/blah")
    )
    assert requires[0].full_name == 'repo-test/pkg-test'
