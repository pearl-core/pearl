import os
import platform
import pytest
import re

from unittest import mock

from pearllib.utils import verify_bash_dep, verify_git_dep, check_and_copy, ask, apply, unapply, run_bash, \
    run_pearl_bash
from test_pearllib.utils import create_pearl_env, create_pearl_home

_MODULE_UNDER_TEST = 'pearllib.utils'


@pytest.mark.parametrize(
    'bash_st, bash_ver, expected_result',
    [
        pytest.param(
            0,
            '5.0.11(1)-release',
            True
        ),
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
        sub_mock = mock.Mock()
        sub_mock.stdout = bash_ver
        sub_mock.returncode = bash_st
        subprocess.run.return_value = sub_mock
        assert verify_bash_dep() == expected_result


def test_bash_command_not_found():
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        with pytest.raises(EnvironmentError):
            sub_mock = mock.Mock()
            sub_mock.stdout = 'bash'
            sub_mock.returncode = 127
            subprocess.run.return_value = sub_mock
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
        sub_mock = mock.Mock()
        sub_mock.stdout = git_ver
        sub_mock.returncode = git_st
        subprocess.run.return_value = sub_mock
        assert verify_git_dep() == expected_result


def test_git_command_not_found():
    with mock.patch(_MODULE_UNDER_TEST + '.subprocess') as subprocess:
        with pytest.raises(EnvironmentError):
            sub_mock = mock.Mock()
            sub_mock.stdout = 'git'
            sub_mock.returncode = 127
            subprocess.run.return_value = sub_mock
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


@pytest.mark.parametrize(
    'script, input, expected_stdout, expected_status',
    [
        pytest.param(
            'echo ciao\nfalse',
            None,
            'ciao\n',
            1
        ),
        pytest.param(
            'echo ciao',
            None,
            'ciao\n',
            0
        ),
        pytest.param(
            'echo ciao\nasfpoji',
            None,
            'ciao\n',
            127
        ),
        pytest.param(
            'read var; echo $var\n',
            "ciao\n",
            'ciao\n',
            0
        ),
        # No confirm
        pytest.param(
            'read var; [[ -z $var ]] && echo ciao; read var2; echo $var2',
            "",
            'ciao\n\n',
            0
        ),
    ]
)
def test_run_bash(script, input, expected_stdout, expected_status):
    result = run_bash(script, input=input, capture_stdout=True, check=False, capture_stderr=True)
    assert result.stdout == expected_stdout
    assert result.returncode == expected_status


def test_run_pearl_bash(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    echo $PEARL_HOME
    echo $PWD
    echo $PATH
    info "Test"
    """
    result = run_pearl_bash(script, pearl_env, capture_stdout=True, capture_stderr=True, enable_xtrace=False)

    assert result.stderr == ''

    if platform.system() == 'Darwin':
        assert result.stdout == "{}\n{}\n{}\n{}\n".format(
            home_dir, home_dir,
            '/usr/local/opt/gnu-sed/libexec/gnubin:'
            '/usr/local/opt/grep/libexec/gnubin:'
            '/usr/local/opt/coreutils/libexec/gnubin:' + os.environ['PATH'],
            "\x1b[0;36mTest\x1b[0m"
        )
    elif platform.system() == 'Linux':
        assert result.stdout == "{}\n{}\n{}\n{}\n".format(
            home_dir, home_dir,
            os.environ['PATH'],
            "\x1b[0;36mTest\x1b[0m"
        )


def test_run_pearl_bash_enable_xtrace(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    pearl_env = create_pearl_env(home_dir, {})

    script = "echo hello"
    result = run_pearl_bash(script, pearl_env, capture_stdout=True, capture_stderr=True, enable_xtrace=True)
    assert re.match(r"\+\s?echo hello\n", result.stderr) is not None


def test_run_pearl_bash_enable_errexit(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    echo hello1
    false
    echo hello2
    """
    result = run_pearl_bash(script, pearl_env, capture_stdout=True, capture_stderr=True, check=False, enable_errexit=True)
    assert "hello1\n" == result.stdout

    result = run_pearl_bash(script, pearl_env, capture_stdout=True, capture_stderr=True, check=False, enable_errexit=False)
    assert "hello1\nhello2\n" == result.stdout


@pytest.mark.parametrize(
    'answers, yes_as_default_answer, expected_result',
    [
        pytest.param(
            ['Y'], False, True
        ),
        pytest.param(
            ['N'], True, False
        ),
        pytest.param(
            ['y'], False, True
        ),
        pytest.param(
            ['n'], True, False
        ),
        pytest.param(
            ['R', 'c', 'q', 'Y'], False, True
        ),
        pytest.param(
            ['R', 'c', 'q', 'N'], False, False
        ),
        pytest.param(
            ['R', ''], False, False
        ),
        pytest.param(
            ['R', ''], True, True
        ),
    ]
)
def test_ask(answers, yes_as_default_answer, expected_result):
    with mock.patch('builtins.input') as input_mock:
        input_mock.side_effect = answers

        assert ask('prompt', yes_as_default_answer) == expected_result


@pytest.mark.parametrize(
    'line, file_content, expected_result',
    [
        pytest.param(
            'This should stay on top',
            '',
            'This should stay on top\n',
        ),
        pytest.param(
            'This should\nstay on top',
            '',
            'This should\nstay on top\n',
        ),
        pytest.param(
            'This should stay on top',
            'First line\nSecond line',
            'This should stay on top\nFirst line\nSecond line',
        ),
        pytest.param(
            'This should stay on top',
            'This should stay on top\nFirst line\nSecond line',
            'This should stay on top\nFirst line\nSecond line',
        ),
        pytest.param(
            'This should stay on top',
            'First line\nThis should stay on top\nSecond line',
            'First line\nThis should stay on top\nSecond line',
        ),
    ]
)
def test_apply(line, file_content, expected_result, tmp_path):
    (tmp_path / 'file').write_text(file_content)

    apply(line, str(tmp_path / 'file'))

    assert (tmp_path / 'file').read_text() == expected_result


def test_apply_file_not_exist(tmp_path):
    line = 'Add this line'
    apply(line, str(tmp_path / 'file'))

    assert (tmp_path / 'file').read_text() == line + '\n'


def test_apply_dir_and_file_not_exist(tmp_path):
    line = 'Add this line'
    apply(line, str(tmp_path / 'mydir/file'))

    assert (tmp_path / 'mydir/file').read_text() == line + '\n'


@pytest.mark.parametrize(
    'line, file_content, expected_result',
    [
        pytest.param(
            'This should stay on top',
            'First line\nSecond line',
            'First line\nSecond line',
        ),
        pytest.param(
            'This should\nstay on top',
            'First line\nThis should\nstay on top\nSecond line',
            'First line\nSecond line',
        ),
        pytest.param(
            'This should stay on top',
            'This should stay on top\nFirst line\nSecond line',
            'First line\nSecond line',
        ),
        pytest.param(
            'This should stay on top',
            'First line\nSecond line\nThis should stay on top',
            'First line\nSecond line\n',
        ),
        pytest.param(
            'This should stay on top',
            'First line\nSecond line\nThis should stay on top\n',
            'First line\nSecond line\n',
        ),
        pytest.param(
            'This should stay on top',
            'First line\nThis should stay on top\nSecond line',
            'First line\nSecond line',
        ),
    ]
)
def test_unapply(line, file_content, expected_result, tmp_path):
    (tmp_path / 'file').write_text(file_content)

    unapply(line, str(tmp_path / 'file'))

    assert (tmp_path / 'file').read_text() == expected_result


def test_unapply_file_not_exist(tmp_path):
    line = 'Add this line'
    unapply(line, str(tmp_path / 'file'))

    assert not (tmp_path / 'file').exists()


def test_unapply_dir_and_file_not_exist(tmp_path):
    line = 'Add this line'
    unapply(line, str(tmp_path / 'mydir/file'))

    assert not (tmp_path / 'file').exists()
