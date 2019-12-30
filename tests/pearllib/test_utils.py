import pytest
from unittest import mock

from pearllib.utils import verify_bash_dep, verify_git_dep, check_and_copy

_MODULE_UNDER_TEST = 'pearllib.utils'


@pytest.mark.parametrize(
    'bash_st, bash_ver, expected_result',
    [
        pytest.param(
            0,
            '4.2',
            True
        ),
        pytest.param(
            0,
            '4.1',
            True
        ),
        pytest.param(
            0,
            '5',
            True
        ),
        pytest.param(
            0,
            '3.9',
            False
        ),
        pytest.param(
            0,
            None,
            False
        ),
    ]
)
def test_verify_bash_dep(bash_st, bash_ver, expected_result):
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        subprocess.getstatusoutput.return_value = bash_st, bash_ver
        assert verify_bash_dep() == expected_result


def test_bash_command_not_found():
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        with pytest.raises(EnvironmentError):
            subprocess.getstatusoutput.return_value = 127, 'bash'
            verify_bash_dep()


@pytest.mark.parametrize(
    'git_st, git_ver, expected_result',
    [
        pytest.param(
            0,
            'git version 1.8.6',
            True
        ),
        pytest.param(
            0,
            'git version 1.8.5',
            True
        ),
        pytest.param(
            0,
            'git version 2',
            True
        ),
        pytest.param(
            0,
            'git version 1.7.9',
            False
        ),
    ]
)
def test_verify_git_dep(git_st, git_ver, expected_result):
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        subprocess.getstatusoutput.return_value = git_st, git_ver
        assert verify_git_dep() == expected_result


def test_git_command_not_found():
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        with pytest.raises(EnvironmentError):
            subprocess.getstatusoutput.return_value = 127, 'git'
            verify_git_dep()


def test_check_and_copy(tmp_path):
    src_dir = tmp_path / 'src'
    (src_dir / 'path1').mkdir(parents=True)
    (src_dir / 'path1/file1').touch()
    dst_dir = tmp_path / 'dst'
    (dst_dir / 'path1').mkdir(parents=True)
    (dst_dir / 'path1/fileold1').touch()
    check_and_copy(src_dir, dst_dir)

    assert not (dst_dir / 'path1/fileold1').exists()
    assert (dst_dir / 'path1/file1').exists()


def test_check_and_copy_not_a_dir(tmp_path):
    with pytest.raises(NotADirectoryError):
        check_and_copy(tmp_path / 'not-a-dir', tmp_path)

