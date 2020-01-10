from unittest import mock

from pearllib.pearlenv import PearlEnvironment


def create_pearl_home(tmp_path):
    home_dir = tmp_path / 'home'
    home_dir.mkdir(parents=True)
    pearl_conf = home_dir / 'pearl.conf'
    pearl_conf.touch()
    return home_dir


def create_pearl_env(home_dir, packages):
    pearl_env = mock.Mock(spec=PearlEnvironment)
    pearl_env.home = home_dir
    pearl_env.packages = packages
    return pearl_env
