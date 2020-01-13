from unittest import mock

import pytest

from pearllib.exceptions import PackageAlreadyInstalledError
from pearllib.pearl import pearl

_MODULE_UNDER_TEST = 'pearllib.pearl'


def expected_pack_calls(install_pkg=0, update_pkg=0, emerge_pkg=0, remove_pkg=0, list_pkg=0, create_pkg=0):
    return {
        'install_package': install_pkg,
        'update_package': update_pkg,
        'emerge_package': emerge_pkg,
        'remove_package': remove_pkg,
        'list_packages': list_pkg,
        'create_package': create_pkg,
    }


def expected_syst_calls(init=0, update=0, remove=0):
    return {
        'init_pearl': init,
        'update_pearl': update,
        'remove_pearl': remove,
    }


@pytest.mark.parametrize(
    'args, expected_pack_call_counts, expected_syst_call_counts',
    [
        pytest.param(
            ['install', 'pkg'],
            expected_pack_calls(install_pkg=1),
            expected_syst_calls()
        ),
        pytest.param(
            ['install', 'pkg1', 'pkg2'],
            expected_pack_calls(install_pkg=2),
            expected_syst_calls()
        ),
        pytest.param(
            ['update', 'pkg1', 'pkg2', 'pkg3'],
            expected_pack_calls(update_pkg=3),
            expected_syst_calls()
        ),
        pytest.param(
            ['emerge', 'pkg1', 'pkg2', 'pkg3'],
            expected_pack_calls(emerge_pkg=3),
            expected_syst_calls()
        ),
        pytest.param(
            ['remove', 'pkg1', 'pkg2', 'pkg3'],
            expected_pack_calls(remove_pkg=3),
            expected_syst_calls()
        ),
        pytest.param(
            ['list'],
            expected_pack_calls(list_pkg=1),
            expected_syst_calls()
        ),
        pytest.param(
            ['search', 'pattern'],
            expected_pack_calls(list_pkg=1),
            expected_syst_calls()
        ),
        pytest.param(
            ['create', 'name', 'dest_dir'],
            expected_pack_calls(create_pkg=1),
            expected_syst_calls()
        ),

        pytest.param(
            ['init'],
            expected_pack_calls(),
            expected_syst_calls(init=1)
        ),
        pytest.param(
            ['update'],
            expected_pack_calls(),
            expected_syst_calls(update=1)
        ),
        pytest.param(
            ['remove'],
            expected_pack_calls(),
            expected_syst_calls(remove=1)
        ),
    ]
)
def test_pearl(args, expected_pack_call_counts, expected_syst_call_counts, tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    (home_dir / 'pearl.conf').touch()

    pearl_home_dir = tmp_path / 'pearlhome'
    pearl_home_dir.mkdir(parents=True)

    with mock.patch(_MODULE_UNDER_TEST + '.pack') as pack_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.syst') as syst_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.verify_runtime_deps') as verify_mock:
        pearl(['-c', str(home_dir / 'pearl.conf')] + args, pearl_home_dir=pearl_home_dir)

        for func_name, count in expected_pack_call_counts.items():
            assert getattr(pack_mock, func_name).call_count == count

        for func_name, count in expected_syst_call_counts.items():
            assert getattr(syst_mock, func_name).call_count == count

        assert verify_mock.call_count == 1


def test_pearl_no_command(tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    (home_dir / 'pearl.conf').touch()

    pearl_home_dir = tmp_path / 'pearlhome'
    pearl_home_dir.mkdir(parents=True)
    with pytest.raises(SystemExit):
        pearl(['-c', str(home_dir / 'pearl.conf')], pearl_home_dir=pearl_home_dir)


@pytest.mark.parametrize(
    'command, func_name',
    [
        pytest.param(
            'install',
            'install_package'
        ),
        pytest.param(
            'update',
            'update_package'
        ),
        pytest.param(
            'emerge',
            'emerge_package'
        ),
        pytest.param(
            'remove',
            'remove_package'
        ),
    ]
)
def test_pearl_error(command, func_name, tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    (home_dir / 'pearl.conf').touch()

    pearl_home_dir = tmp_path / 'pearlhome'
    pearl_home_dir.mkdir(parents=True)

    with mock.patch(_MODULE_UNDER_TEST + '.pack') as pack_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.syst'), \
            mock.patch(_MODULE_UNDER_TEST + '.verify_runtime_deps') as verify_mock:

        def side_eff(_, package, options):
            if package == 'pkg1':
                raise PackageAlreadyInstalledError('Error!')
        getattr(pack_mock, func_name).side_effect = side_eff

        with pytest.raises(SystemExit) as exc:
            pearl(['-c', str(home_dir / 'pearl.conf'), command, 'pkg1', 'pkg2'], pearl_home_dir=pearl_home_dir)
            assert exc.value.code == 102
            assert getattr(pack_mock, func_name).call_count == 2
            assert verify_mock.call_count == 1
