from pathlib import Path
from unittest import mock

import pkg_resources

from pearllib.pearlenv import PearlEnvironment, PearlOptions
from pearllib.system import init_pearl, remove_pearl, update_pearl

_MODULE_UNDER_TEST = 'pearllib.system'


def test_init(tmp_path):
    (tmp_path / 'home').mkdir(parents=True)

    pearl_env = mock.Mock(spec=PearlEnvironment)
    pearl_env.home = (tmp_path / 'pearlhome')
    (pearl_env.home / 'bin').mkdir(parents=True)
    (pearl_env.home / 'bin/pearl').touch()

    pearl_env.root = (tmp_path / 'pearlroot')
    (pearl_env.root / 'bin').mkdir(parents=True)
    (pearl_env.root / 'etc').mkdir(parents=True)
    (pearl_env.root / 'bin/pearl').touch()
    (pearl_env.root / 'etc/pearl.conf.template').touch()

    with mock.patch(_MODULE_UNDER_TEST + '.os') as os_mock:
        os_mock.environ = {
            'HOME': str(tmp_path / 'home')
        }
        init_pearl(pearl_env)

        assert (pearl_env.home / 'bin').is_dir()
        assert (pearl_env.home / 'packages').is_dir()
        assert (pearl_env.home / 'repos').is_dir()
        assert (pearl_env.home / 'tmp').is_dir()
        assert (pearl_env.home / 'var').is_dir()

        assert (pearl_env.home / 'bin/pearl').is_symlink()
        assert (pearl_env.home / 'pearl.conf').is_file()

        static = Path(pkg_resources.resource_filename('pearllib', 'static/'))
        assert (tmp_path / 'home/.bashrc').read_text() == "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
        assert (tmp_path / 'home/.zshrc').read_text() == "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
        assert (tmp_path / 'home/.config/fish/config.fish').read_text() == "set -x PEARL_ROOT {}\nsource {}/boot/fish/pearl.fish\n".format(pearl_env.root, static)
        assert (tmp_path / 'home/.vimrc').read_text() == "source {}/boot/vim/pearl.vim\n".format(static)
        assert (tmp_path / 'home/.emacs').read_text() == "source {}/boot/emacs/pearl.el\n".format(static)


def test_remove(tmp_path):
    (tmp_path / 'home/.config/fish').mkdir(parents=True)

    pearl_env = mock.Mock()
    pearl_env.home = (tmp_path / 'pearlhome')
    pearl_env.home.mkdir(parents=True)

    pearl_env.root = (tmp_path / 'pearlroot')
    pearl_env.root.mkdir(parents=True)

    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))

    (tmp_path / 'home/.bashrc').write_text(
        "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.zshrc').write_text(
        "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.config/fish/config.fish').write_text(
        "set -x PEARL_ROOT {}\nsource {}/boot/fish/pearl.fish\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.vimrc').write_text(
        "source {}/boot/vim/pearl.vim\n".format(static)
    )
    (tmp_path / 'home/.emacs').write_text(
        "source {}/boot/emacs/pearl.el\n".format(static)
    )

    pearl_env.packages = {}

    with mock.patch(_MODULE_UNDER_TEST + '.os') as os_mock, \
            mock.patch('builtins.input') as input_mock:
        os_mock.environ = {
            'HOME': str(tmp_path / 'home')
        }
        input_mock.return_value = 'Y'
        remove_pearl(pearl_env)

        assert not pearl_env.home.exists()

        assert (tmp_path / 'home/.bashrc').read_text() == ""
        assert (tmp_path / 'home/.zshrc').read_text() == ""
        assert (tmp_path / 'home/.config/fish/config.fish').read_text() == ""
        assert (tmp_path / 'home/.vimrc').read_text() == ""
        assert (tmp_path / 'home/.emacs').read_text() == ""


def test_remove_no_confirm(tmp_path):
    (tmp_path / 'home/.config/fish').mkdir(parents=True)

    pearl_env = mock.Mock()
    pearl_env.home = (tmp_path / 'pearlhome')
    pearl_env.home.mkdir(parents=True)

    pearl_env.root = (tmp_path / 'pearlroot')
    pearl_env.root.mkdir(parents=True)

    static = Path(pkg_resources.resource_filename('pearllib', 'static/'))

    (tmp_path / 'home/.bashrc').write_text(
        "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.zshrc').write_text(
        "export PEARL_ROOT={}\nsource {}/boot/sh/pearl.sh\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.config/fish/config.fish').write_text(
        "set -x PEARL_ROOT {}\nsource {}/boot/fish/pearl.fish\n".format(pearl_env.root, static)
    )
    (tmp_path / 'home/.vimrc').write_text(
        "source {}/boot/vim/pearl.vim\n".format(static)
    )
    (tmp_path / 'home/.emacs').write_text(
        "source {}/boot/emacs/pearl.el\n".format(static)
    )

    pearl_env.packages = {}

    with mock.patch(_MODULE_UNDER_TEST + '.os') as os_mock:
        os_mock.environ = {
            'HOME': str(tmp_path / 'home')
        }
        remove_pearl(pearl_env, options=PearlOptions(True, 0))

        assert not pearl_env.home.exists()

        assert (tmp_path / 'home/.bashrc').read_text() == ""
        assert (tmp_path / 'home/.zshrc').read_text() == ""
        assert (tmp_path / 'home/.config/fish/config.fish').read_text() == ""
        assert (tmp_path / 'home/.vimrc').read_text() == ""
        assert (tmp_path / 'home/.emacs').read_text() == ""


def test_update(tmp_path):
    pearl_env = mock.Mock()

    pearl_env.root = (tmp_path / 'pearlroot')
    pearl_env.root.mkdir(parents=True)

    pearl_env.packages = {}

    with mock.patch('builtins.input') as input_mock, \
            mock.patch(_MODULE_UNDER_TEST + '.run_bash') as run_mock:
        input_mock.return_value = 'Y'
        update_pearl(pearl_env)

        assert run_mock.call_count == 1


def test_update_no_confirm(tmp_path):
    pearl_env = mock.Mock()

    pearl_env.root = (tmp_path / 'pearlroot')
    pearl_env.root.mkdir(parents=True)

    pearl_env.packages = {}

    with mock.patch(_MODULE_UNDER_TEST + '.run_bash') as run_mock:
        update_pearl(pearl_env, options=PearlOptions(True, 0))

        assert run_mock.call_count == 1




