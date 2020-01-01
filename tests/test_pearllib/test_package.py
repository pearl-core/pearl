from unittest import mock

import pytest

from pearllib.exceptions import RepoDoesNotExistError, PackageNotInRepoError, PackageAlreadyInstalledError, \
    HookFunctionError
from pearllib.package import install_package
from pearllib.pearlenv import Package

from .utils import create_pearl_env, create_pearl_home, create_pearl_root

_MODULE_UNDER_TEST = 'pearllib.package'


def _create_local_packages(
        tmp_path, home_dir, post_install,
        repo_name='repo-test',
        package_name='pkg-test'
):
    pkg_dir = tmp_path / 'pkg'
    (pkg_dir / 'pearl-config').mkdir(parents=True)
    install_sh = pkg_dir / 'pearl-config/install.sh'
    install_sh.touch()
    install_sh.write_text(post_install)

    package = Package(home_dir, repo_name, package_name, str(pkg_dir), '')
    packages = {
        repo_name: {
            package_name: package
        }
    }
    return packages


def _create_git_packages(
        home_dir,
        repo_name='repo-test',
        package_name='pkg-test'
):
    package = Package(home_dir, repo_name, package_name, 'https://github.com/pkg', '')
    packages = {
        repo_name: {
            package_name: package
        }
    }
    return packages


def test_install_local_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)
    post_install = """
    post_install() {{
        echo $PEARL_HOME > {homedir}/result
        echo $PEARL_ROOT >> {homedir}/result
        echo $PEARL_PKGDIR >> {homedir}/result
        echo $PEARL_PKGVARDIR >> {homedir}/result
        echo $PEARL_PKGNAME >> {homedir}/result
        echo $PEARL_PKGREPONAME >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir)

    packages = _create_local_packages(tmp_path, home_dir, post_install)
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, root_dir, packages)

    install_package(pearl_env, 'repo-test/pkg-test')

    assert (home_dir / 'packages/repo-test/pkg-test/pearl-config/install.sh').is_file()
    assert (home_dir / 'var/repo-test/pkg-test').is_dir()
    assert (home_dir / 'result').is_file()
    expected_result = """{}\n{}\n{}\n{}\n{}\n{}\n""".format(home_dir, root_dir, package.dir, package.vardir, package.name, package.repo_name)
    assert (home_dir / 'result').read_text() == expected_result


def test_install_package_git(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)

    packages = _create_git_packages(home_dir)
    pearl_env = create_pearl_env(home_dir, root_dir, packages)

    with mock.patch(_MODULE_UNDER_TEST + ".run") as run_mock:
        install_package(pearl_env, 'repo-test/pkg-test')

        assert run_mock.call_count == 2
        assert (home_dir / 'var/repo-test/pkg-test').is_dir()


def test_install_package_raise_hook(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)
    post_install = """
    post_install() {{
        command-notfound
        return 0
    }}
    """

    packages = _create_local_packages(tmp_path, home_dir, post_install)
    pearl_env = create_pearl_env(home_dir, root_dir, packages)

    with pytest.raises(HookFunctionError):
        install_package(pearl_env, 'repo-test/pkg-test')


def test_install_package_repo_not_exist(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)
    pearl_env = create_pearl_env(home_dir, root_dir, {})

    with pytest.raises(RepoDoesNotExistError):
        install_package(pearl_env, 'test/pkg-test')


def test_install_package_package_not_exist(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)

    packages = _create_local_packages(tmp_path, home_dir, "")
    pearl_env = create_pearl_env(home_dir, root_dir, packages)
    with pytest.raises(PackageNotInRepoError):
        install_package(pearl_env, 'repo-test/pkg-a-test')

    with pytest.raises(PackageNotInRepoError):
        install_package(pearl_env, 'pkg-a-test')


def test_install_package_already_installed(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    root_dir = create_pearl_root(tmp_path)

    packages = _create_local_packages(tmp_path, home_dir, "")
    pearl_env = create_pearl_env(home_dir, root_dir, packages)

    (home_dir / 'packages/repo-test/pkg-test').mkdir(parents=True)

    with pytest.raises(PackageAlreadyInstalledError):
        install_package(pearl_env, 'repo-test/pkg-test')

