from pathlib import Path
from unittest import mock

from pearllib.package import install_package
from pearllib.pearlenv import Package, PearlEnvironment


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
