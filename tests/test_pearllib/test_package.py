from unittest import mock

import pytest

from pearllib.exceptions import RepoDoesNotExistError, PackageNotInRepoError, PackageAlreadyInstalledError
from pearllib.package import install_package
from pearllib.pearlenv import Package, PearlEnvironment
from test_pearllib.utils import create_pearl_env


def test_install_package(tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    pkg_dir = tmp_path / 'pkg'
    (pkg_dir / 'pearl-config').mkdir(parents=True)
    install_sh = pkg_dir / 'pearl-config/install.sh'
    install_sh.touch()
    install_sh.write_text("""
    post_install() {{
        echo $PEARL_PKGDIR > {homedir}/result
        echo $PEARL_PKGVARDIR >> {homedir}/result
        echo $PEARL_PKGNAME >> {homedir}/result
        echo $PEARL_PKGREPONAME >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir))

    package = Package(home_dir, 'test', 'pkg-test', str(pkg_dir), '')
    packages = {
        'test': {
            'pkg-test': package
        }
    }
    pearl_env = mock.Mock(spec=PearlEnvironment)
    pearl_env.home = home_dir
    pearl_env.packages = packages
    install_package(pearl_env, 'test/pkg-test')

    assert (home_dir / 'packages/test/pkg-test/pearl-config/install.sh').is_file()
    assert (home_dir / 'var/test/pkg-test').is_dir()
    assert (home_dir / 'result').is_file()
    expected_result = """{}\n{}\n{}\n{}\n""".format(package.dir, package.vardir, package.name, package.repo_name)
    assert (home_dir / 'result').read_text() == expected_result


def test_install_package_repo_not_exist(tmp_path):
    pearl_home = tmp_path / 'home'
    pearl_home.mkdir()
    pearl_root = tmp_path / 'root'
    pearl_root.mkdir()

    pearl_conf = pearl_home / 'pearl.conf'
    pearl_conf.write_text("")
    pearl_env = create_pearl_env(pearl_root, pearl_home, pearl_conf)
    with pytest.raises(RepoDoesNotExistError):
        install_package(pearl_env, 'test/pkg-test')


def test_install_package_package_not_exist(tmp_path):
    pearl_home = tmp_path / 'home'
    pearl_home.mkdir()
    pearl_root = tmp_path / 'root'
    pearl_root.mkdir()

    pearl_conf = pearl_home / 'pearl.conf'
    pearl_conf.write_text("""
PEARL_REPO_NAME = 'test'
PEARL_PACKAGES = {
    'a-test-pkg': {
        'url': 'https://github.com/username/test-pkg',
        'description': 'My descr'
    },
}
    """)
    pearl_env = create_pearl_env(pearl_root, pearl_home, pearl_conf)
    with pytest.raises(PackageNotInRepoError):
        install_package(pearl_env, 'test/pkg-test')

    with pytest.raises(PackageNotInRepoError):
        install_package(pearl_env, 'pkg-test')


def test_install_package_already_installed(tmp_path):
    pearl_home = tmp_path / 'home'
    (pearl_home / 'packages/test/test-pkg').mkdir(parents=True)
    pearl_root = tmp_path / 'root'
    pearl_root.mkdir()

    pearl_conf = pearl_home / 'pearl.conf'
    pearl_conf.write_text("""
PEARL_REPO_NAME = 'test'
PEARL_PACKAGES = {
    'test-pkg': {
        'url': 'https://github.com/username/test-pkg',
        'description': 'My descr'
    },
}
    """)
    pearl_env = create_pearl_env(pearl_root, pearl_home, pearl_conf)
    with pytest.raises(PackageAlreadyInstalledError):
        install_package(pearl_env, 'test/test-pkg')

