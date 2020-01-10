from pearllib.utils import run_pearl_bash
from test_pearllib.utils import create_pearl_home, create_pearl_env


def test_link_to_path_null_executable_path(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    link_to_path ""
    """
    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)
    assert result.returncode == 11


def test_link_to_path(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'bin').mkdir()
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    echo "Content" > {tmppath}/binary
    link_to_path "{tmppath}/binary"
    cat $PEARL_HOME/bin/binary
    """.format(tmppath=tmp_path)

    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)
    assert result.stdout == "Content\n"
    assert result.returncode == 0


def test_link_to_path_new_executable_name(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'bin').mkdir()
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    echo "Content" > {tmppath}/binary
    link_to_path "{tmppath}/binary" "new_binary"
    cat $PEARL_HOME/bin/new_binary
    """.format(tmppath=tmp_path)

    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)
    assert result.stdout == "Content\n"
    assert result.returncode == 0


def test_unlink_from_path_null_executable_path(tmp_path):
    home_dir = create_pearl_home(tmp_path)
    pearl_env = create_pearl_env(home_dir, {})

    script = """
    unlink_from_path ""
    """
    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)
    assert result.returncode == 11


def test_unlink_from_path(tmp_path):
    (tmp_path / 'binary').write_text("Content")

    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'bin').mkdir()
    (home_dir / 'bin/binary').symlink_to(tmp_path / 'binary')

    pearl_env = create_pearl_env(home_dir, {})

    assert (home_dir / 'bin/binary').exists()

    script = """
    unlink_from_path "{tmppath}/binary"
    """.format(tmppath=tmp_path)

    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)

    assert result.returncode == 0
    assert not (home_dir / 'bin/binary').exists()


def test_unlink_from_path_new_executable_name(tmp_path):
    (tmp_path / 'binary').write_text("Content")

    home_dir = create_pearl_home(tmp_path)
    (home_dir / 'bin').mkdir()
    (home_dir / 'bin/new_binary').symlink_to(tmp_path / 'binary')

    pearl_env = create_pearl_env(home_dir, {})

    assert (home_dir / 'bin/new_binary').exists()

    script = """
    unlink_from_path "{tmppath}/binary" "new_binary"
    """.format(tmppath=tmp_path)

    result = run_pearl_bash(script, pearl_env, capture_stdout=True, check=False)

    assert result.returncode == 0
    assert not (home_dir / 'bin/new_binary').exists()
