from unittest import mock

import pytest

from pearllib.exceptions import PackageAlreadyInstalledError, \
    HookFunctionError, PackageNotInstalledError, PackageRequiredByOtherError
from pearllib.package import install_package, remove_package, list_packages, update_package, emerge_package, \
    create_package, info_package, install_packages, update_packages, emerge_packages, \
    remove_packages, info_packages, closure_dependency_tree
from pearllib.pearlenv import Package, PearlEnvironment, PackageBuilder

from .utils import create_pearl_env, create_pearl_home, PackageTestBuilder, PackageArgs

_MODULE_UNDER_TEST = 'pearllib.package'


def test_install_local_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    post_install() {{
        echo $PWD > {homedir}/result
        echo $PEARL_HOME >> {homedir}/result
        echo $PEARL_PKGDIR >> {homedir}/result
        echo $PEARL_PKGVARDIR >> {homedir}/result
        echo $PEARL_PKGNAME >> {homedir}/result
        echo $PEARL_PKGREPONAME >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    install_package(pearl_env, package, PackageArgs(verbose=2))

    assert (home_dir / 'packages/repo-test/pkg-test/pearl-config/hooks.sh').is_file()
    assert (home_dir / 'var/repo-test/pkg-test').is_dir()

    expected_result = """{}\n{}\n{}\n{}\n{}\n{}\n""".format(
        package.dir, home_dir, package.dir, package.vardir,
        package.name, package.repo_name
    )
    assert (home_dir / 'result').read_text() == expected_result


def test_install_local_package_forced(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    post_install() {{
        return 11
    }}
    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    install_package(pearl_env, package, args=PackageArgs(force=True))

    # Because rollback did not occur:
    assert (home_dir / 'packages/repo-test/pkg-test/').exists()
    assert (home_dir / 'var/repo-test/pkg-test').is_dir()


def test_install_local_package_no_confirm(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    post_install() {{
        if ask "Are you sure?" "Y"
        then
            echo "YES" > {homedir}/result
        else
            echo "NO" > {homedir}/result
        fi
        local choice=$(choose "What?" "banana" "apple" "banana" "orange")
        echo "$choice" >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    install_package(pearl_env, package, PackageArgs(no_confirm=True, verbose=False))

    assert (home_dir / 'result').read_text() == "YES\nbanana\n"


def test_install_package_git(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_git_package("", is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with mock.patch(_MODULE_UNDER_TEST + ".run_pearl_bash") as run_mock:
        install_package(pearl_env, package, PackageArgs())

        assert run_mock.call_count == 2
        assert (home_dir / 'var/repo-test/pkg-test').is_dir()


def test_install_package_raise_hook(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    post_install() {
        command-notfound
        return 0
    }
    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(HookFunctionError):
        install_package(pearl_env, package, PackageArgs())

    # Because of rollback:
    assert not (home_dir / 'packages/repo-test/pkg-test').exists()
    assert (home_dir / 'var/repo-test/pkg-test').is_dir()


def test_install_package_already_installed(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, "", is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(PackageAlreadyInstalledError):
        install_package(pearl_env, package, PackageArgs())


def test_update_local_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    pre_update() {{
        echo $PWD > {homedir}/result
        echo $PEARL_HOME >> {homedir}/result
        echo $PEARL_PKGDIR >> {homedir}/result
        echo $PEARL_PKGVARDIR >> {homedir}/result
        echo $PEARL_PKGNAME >> {homedir}/result
        echo $PEARL_PKGREPONAME >> {homedir}/result
        return 0
    }}
    post_update() {{
        echo $PWD > {homedir}/result2
        echo $PEARL_HOME >> {homedir}/result2
        echo $PEARL_PKGDIR >> {homedir}/result2
        echo $PEARL_PKGVARDIR >> {homedir}/result2
        echo $PEARL_PKGNAME >> {homedir}/result2
        echo $PEARL_PKGREPONAME >> {homedir}/result2
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    update_package(pearl_env, package, PackageArgs(verbose=2))

    assert (home_dir / 'packages/repo-test/pkg-test/pearl-config/hooks.sh').is_file()

    expected_result = """{}\n{}\n{}\n{}\n{}\n{}\n""".format(
        package.dir, home_dir, package.dir, package.vardir,
        package.name, package.repo_name
    )
    assert (home_dir / 'result').read_text() == expected_result

    expected_result = """{}\n{}\n{}\n{}\n{}\n{}\n""".format(
        package.dir, home_dir, package.dir, package.vardir,
        package.name, package.repo_name
    )
    assert (home_dir / 'result2').read_text() == expected_result


def test_update_local_package_forced(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    pre_update() {{
        return 11
    }}

    post_update() {{
        return 12
    }}

    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    update_package(pearl_env, package, args=PackageArgs(False, 0, force=True))

    with pytest.raises(HookFunctionError):
        update_package(pearl_env, package, args=PackageArgs(False, 0, force=False))

    assert (home_dir / 'packages/repo-test/pkg-test/pearl-config/hooks.sh').is_file()


def test_update_local_package_no_confirm(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    pre_update() {{
        if ask "Are you sure?" "Y"
        then
            echo "YES" > {homedir}/result
        else
            echo "NO" > {homedir}/result
        fi

        local choice=$(choose "What?" "banana" "apple" "banana" "orange")
        echo "$choice" >> {homedir}/result
        return 0
    }}
    post_update() {{
        if ask "Are you sure?" "N"
        then
            echo "YES" > {homedir}/result2
        else
            echo "NO" > {homedir}/result2
        fi

        local choice=$(choose "What?" "orange" "apple" "banana" "orange")
        echo "$choice" >> {homedir}/result2
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    update_package(pearl_env, package, PackageArgs(no_confirm=True, verbose=False))

    assert (home_dir / 'result').read_text() == "YES\nbanana\n"
    assert (home_dir / 'result2').read_text() == "NO\norange\n"


def test_update_package_git_url_not_changed(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_git_package("", is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with mock.patch(_MODULE_UNDER_TEST + ".run_pearl_bash"), \
            mock.patch(_MODULE_UNDER_TEST + ".remove_package") as remove_mock, \
            mock.patch(_MODULE_UNDER_TEST + ".install_package") as install_mock:
        update_package(pearl_env, package, PackageArgs())

        assert remove_mock.call_count == 0
        assert install_mock.call_count == 0


def test_update_package_git_url_changed(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_git_package(
        "",
        is_installed=True,
        url='https://github.com/new-pkg',
        git_url='https://github.com/pkg',
    )
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with mock.patch(_MODULE_UNDER_TEST + ".run_pearl_bash"), \
            mock.patch(_MODULE_UNDER_TEST + ".remove_package") as remove_mock, \
            mock.patch(_MODULE_UNDER_TEST + ".install_package") as install_mock:
        update_package(pearl_env, package, PackageArgs())

        assert remove_mock.call_count == 1
        assert install_mock.call_count == 1


def test_update_package_raise_hook(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    pre_update() {{
        command-notfound
        return 0
    }}
    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(HookFunctionError):
        update_package(pearl_env, package, PackageArgs())


def test_update_package_not_installed(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, "", is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(PackageNotInstalledError):
        update_package(pearl_env, package, PackageArgs())


def test_emerge_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, "", package_name='pkg-a-test', is_installed=False)
    builder.add_local_package(tmp_path, "", package_name='pkg-b-test', is_installed=True)
    packages = builder.build()
    package_a = packages['repo-test']['pkg-a-test']
    package_b = packages['repo-test']['pkg-b-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with mock.patch(_MODULE_UNDER_TEST + ".update_package") as update_mock, \
            mock.patch(_MODULE_UNDER_TEST + ".install_package") as install_mock:
        emerge_package(pearl_env, package_a, PackageArgs())
        assert update_mock.call_count == 0
        assert install_mock.call_count == 1

    with mock.patch(_MODULE_UNDER_TEST + ".update_package") as update_mock, \
            mock.patch(_MODULE_UNDER_TEST + ".install_package") as install_mock:
        emerge_package(pearl_env, package_b, PackageArgs())
        assert update_mock.call_count == 1
        assert install_mock.call_count == 0


def test_remove_package(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    hooks_sh_script = """
    pre_remove() {{
        echo $PWD > {homedir}/result
        echo $PEARL_HOME >> {homedir}/result
        echo $PEARL_PKGDIR >> {homedir}/result
        echo $PEARL_PKGVARDIR >> {homedir}/result
        echo $PEARL_PKGNAME >> {homedir}/result
        echo $PEARL_PKGREPONAME >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    remove_package(pearl_env, package, PackageArgs(verbose=2))

    assert not (home_dir / 'packages/repo-test/pkg-test/').exists()

    expected_result = """{}\n{}\n{}\n{}\n{}\n{}\n""".format(
        package.dir, home_dir,
        package.dir, package.vardir, package.name, package.repo_name
    )
    assert (home_dir / 'result').read_text() == expected_result


def test_remove_package_forced(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    hooks_sh_script = """
    pre_remove() {{
        return 11
    }}
    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    remove_package(pearl_env, package, args=PackageArgs(False, 0, force=True))

    assert not (home_dir / 'packages/repo-test/pkg-test/').exists()


def test_remove_package_no_confirm(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    hooks_sh_script = """
    pre_remove() {{
        if ask "Are you sure?" "Y"
        then
            echo "YES" > {homedir}/result
        else
            echo "NO" > {homedir}/result
        fi

        local choice=$(choose "What?" "banana" "apple" "banana" "orange")
        echo "$choice" >> {homedir}/result
        return 0
    }}
    """.format(homedir=home_dir)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']

    pearl_env = create_pearl_env(home_dir, packages)

    remove_package(pearl_env, package, PackageArgs(no_confirm=True, verbose=False))

    assert (home_dir / 'result').read_text() == "YES\nbanana\n"


def test_remove_package_raise_hook(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    hooks_sh_script = """
    pre_remove() {{
        command-notfound
        return 0
    }}
    """

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, hooks_sh_script, is_installed=True)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(HookFunctionError):
        remove_package(pearl_env, package, PackageArgs())


def test_remove_package_not_installed(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    builder = PackageTestBuilder(home_dir)
    builder.add_local_package(tmp_path, "", is_installed=False)
    packages = builder.build()
    package = packages['repo-test']['pkg-test']
    pearl_env = create_pearl_env(home_dir, packages)

    with pytest.raises(PackageNotInstalledError):
        remove_package(pearl_env, package, PackageArgs())


def test_list_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'packages/repo-test/pkg-a-test').mkdir(parents=True)

    pearl_env = mock.Mock()
    pearl_env.packages = {
        'repo-test': {
            'pkg-a-test': Package(home_dir, 'repo-test', 'pkg-a-test', 'url', 'descr'),
            'pkg-b-test': Package(home_dir, 'repo-test', 'pkg-b-test', 'url', 'descr'),
        }
    }
    result = list_packages(pearl_env, PackageArgs(pattern='pkg'))
    assert len(result) == 2
    assert 'pkg-a-test' in [pkg.name for pkg in result]
    assert 'pkg-b-test' in [pkg.name for pkg in result]


def test_list_packages_match_keyword(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'packages/repo-test/pkg-a-test').mkdir(parents=True)

    pearl_env = mock.Mock()
    pearl_env.packages = {
        'repo-test': {
            'pkg-a-test': Package(home_dir, 'repo-test', 'pkg-a-test', 'url', 'descr', keywords=('pkg', 'pkg-manager')),
            'pkg-b-test': Package(home_dir, 'repo-test', 'pkg-b-test', 'url', 'descr', keywords=('pkg',)),
        }
    }
    result = list_packages(pearl_env, PackageArgs(pattern='pkg-manager'))
    assert len(result) == 1
    assert result[0].name == 'pkg-a-test'


def test_list_packages_not_matching(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'packages/repo-test/pkg-a-test').mkdir(parents=True)

    pearl_env = mock.Mock()
    pearl_env.packages = {
        'repo-test': {
            'pkg-a-test': Package(home_dir, 'repo-test', 'pkg-a-test', 'url', 'descr'),
            'pkg-b-test': Package(home_dir, 'repo-test', 'pkg-b-test', 'url', 'descr'),
        }
    }
    result = list_packages(pearl_env, PackageArgs(pattern='pkg2'))
    assert result == []


def test_create_package(tmp_path):
    dest_dir = tmp_path / 'new-pkg'
    dest_dir.mkdir(parents=True)
    config_file = tmp_path / 'pearl.conf'

    pearl_env = mock.Mock()
    pearl_env.config_filename = config_file

    create_package(
        pearl_env,
        PackageArgs(
            name="mypkg",
            dest_dir=dest_dir
        )
    )

    assert (dest_dir / 'pearl-config').exists()
    assert config_file.read_text() == 'PEARL_PACKAGES["mypkg"] = {{"url": "{}"}}\n'.format(dest_dir)


def test_create_package_pearl_config_exists(tmp_path):
    dest_dir = tmp_path / 'new-pkg'
    (dest_dir / 'pearl-config').mkdir(parents=True)
    config_file = tmp_path / 'pearl.conf'

    pearl_env = mock.Mock()
    pearl_env.config_filename = config_file

    with pytest.raises(RuntimeError):
        create_package(
            pearl_env,
            PackageArgs(
                name="mypkg",
                dest_dir=dest_dir
            )
        )


@pytest.mark.parametrize(
    'initial_package_list, package_deps, expected_result',
    [
        pytest.param(
            ["A"],
            {
                "A": [],
            },
            ["A"]
        ),
        pytest.param(
            ["A"],
            {
                "A": ["B"],
                "B": [],
            },
            ["B", "A"]
        ),
        pytest.param(
            ["A", "B"],
            {
                "A": ["C"],
                "B": ["C"],
                "C": [],
            },
            ["C", "A", "B"]
        ),
        pytest.param(
            ["B", "A", "D"],
            {
                "A": ["B"],
                "B": ["C"],
                "C": [],
                "D": ["C"],
            },
            ["C", "B", "A", "D"]
        ),
        # Cycle
        pytest.param(
            ["A"],
            {
                "A": ["B"],
                "B": ["A"],
            },
            ["B", "A"]
        ),
        pytest.param(
            ["A"],
            {
                "A": ["B"],
                "B": ["C"],
                "C": [],
            },
            ["C", "B", "A"]
        ),
        pytest.param(
            ["C", "B", "A"],
            {
                "A": ["B"],
                "B": ["C"],
                "C": [],
            },
            ["C", "B", "A"]
        ),
        # Duplicates
        pytest.param(
            ["C", "B", "A", "A"],
            {
                "A": ["B"],
                "B": ["C"],
                "C": [],
            },
            ["C", "B", "A"]
        ),
        pytest.param(
            ["C", "B", "A"],
            {
                "A": ["B"],
                "B": [],
                "C": [],
            },
            ["C", "B", "A"]
        ),
    ]
)
def test_closure_dependency_tree(
        tmp_path,
        initial_package_list, package_deps,
        expected_result
):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        'repo-test': {
        }
    }
    for package_name, depends in package_deps.items():
        packages_info['repo-test'][package_name] = {
            'repo_name': 'repo-test',
            'name': package_name,
            'url': '/sdaf',
            'depends': ['repo-test/' + dep for dep in depends]
        }
    builder = PackageBuilder(home_dir)
    packages = builder.build_packages(packages_info)
    init_packages = [packages['repo-test'][name] for name in initial_package_list]
    assert closure_dependency_tree(
        init_packages,
    ) == [packages['repo-test'][name] for name in expected_result]


@pytest.mark.parametrize(
    'is_installed, expected_result',
    [
        pytest.param(True, ("A",)),
        pytest.param(False, ("A",)),
    ]
)
def test_info_package(tmp_path, is_installed, expected_result):
    home_dir = create_pearl_home(tmp_path)

    package_deps = {
        "A": ["B"],
        "B": []
    }
    builder = PackageTestBuilder(home_dir)
    for package_name, depends in package_deps.items():
        builder.add_local_package(
            tmp_path, "",
            package_name=package_name,
            depends=depends,
            is_installed=is_installed,
        )
    packages = builder.build()
    package = packages["repo-test"]["B"]
    pearl_env = create_pearl_env(home_dir, packages)
    pearl_env.required_by.return_value = []
    info_package(pearl_env, package, None)


def test_install_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "pkg2": {
                "repo_name": "repo-test",
                "name": "pkg2",
                "url": "/blah",
                "depends": []
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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

    with mock.patch(_MODULE_UNDER_TEST + '.install_package') as install_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.emerge_package') as emerge_mock:
        args = PackageArgs(packages=[packages['repo-test']['pkg1'], packages['repo-test']['pkg2']])
        install_packages(pearl_env, args)

        emerge_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['deppkg1'], args)])
        install_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['pkg1'], args), mock.call(mock.ANY, packages['repo-test']['pkg2'], args)])


def test_update_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "pkg2": {
                "repo_name": "repo-test",
                "name": "pkg2",
                "url": "/blah",
                "depends": []
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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

    with mock.patch(_MODULE_UNDER_TEST + '.update_package') as update_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.emerge_package') as emerge_mock:
        args = PackageArgs(packages=[packages['repo-test']['pkg1'], packages['repo-test']['pkg2']])
        update_packages(pearl_env, args)

        emerge_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['deppkg1'], args)])
        update_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['pkg1'], args), mock.call(mock.ANY, packages['repo-test']['pkg2'], args)])


def test_emerge_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "pkg2": {
                "repo_name": "repo-test",
                "name": "pkg2",
                "url": "/blah",
                "depends": []
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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

    with mock.patch(_MODULE_UNDER_TEST + '.emerge_package') as emerge_mock:
        args = PackageArgs(packages=[packages['repo-test']['pkg1'], packages['repo-test']['pkg2']])
        emerge_packages(pearl_env, args)

        emerge_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['deppkg1'], args), mock.call(mock.ANY, packages['repo-test']['pkg1'], args), mock.call(mock.ANY, packages['repo-test']['pkg2'], args)])


def test_remove_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "pkg2": {
                "repo_name": "repo-test",
                "name": "pkg2",
                "url": "/blah",
                "depends": []
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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
    with mock.patch(_MODULE_UNDER_TEST + '.remove_package') as remove_mock:
        args = PackageArgs(packages=[packages['repo-test']['pkg1'], packages['repo-test']['pkg2']])
        remove_packages(pearl_env, args)

        remove_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['pkg2'], args), mock.call(mock.ANY, packages['repo-test']['pkg1'], args)])


@pytest.mark.parametrize(
    'required_package_installed, expected_remove_calls',
    [
        pytest.param(True, 0),
        pytest.param(False, 1),
    ]
)
def test_remove_packages_required_installed_packages_raise(tmp_path, required_package_installed, expected_remove_calls):
    home_dir = create_pearl_home(tmp_path)
    if required_package_installed:
        (home_dir / 'packages/repo-test/pkg1').mkdir(parents=True)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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
    with mock.patch(_MODULE_UNDER_TEST + '.remove_package') as remove_mock:
        args = PackageArgs(packages=[packages['repo-test']['deppkg1']])
        if required_package_installed:
            with pytest.raises(PackageRequiredByOtherError):
                remove_packages(pearl_env, args)
        else:
            remove_packages(pearl_env, args)

        assert remove_mock.call_count == expected_remove_calls


def test_info_packages(tmp_path):
    home_dir = create_pearl_home(tmp_path)

    packages_info = {
        "repo-test": {
            "pkg1": {
                "repo_name": "repo-test",
                "name": "pkg1",
                "url": "/blah",
                "depends": ["repo-test/deppkg1"]
            },
            "pkg2": {
                "repo_name": "repo-test",
                "name": "pkg2",
                "url": "/blah",
                "depends": []
            },
            "deppkg1": {
                "repo_name": "repo-test",
                "name": "deppkg1",
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

    with mock.patch(_MODULE_UNDER_TEST + '.info_package') as info_mock:
        args = PackageArgs(packages=[packages['repo-test']['pkg1'], packages['repo-test']['pkg2']])
        info_packages(pearl_env, args)

        info_mock.assert_has_calls([mock.call(mock.ANY, packages['repo-test']['pkg1'], args), mock.call(mock.ANY, packages['repo-test']['pkg2'], args)])
