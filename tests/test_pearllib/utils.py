from unittest import mock

from pearllib.pearlenv import PearlEnvironment


def create_pearl_env(root, home, config_filename):
    with mock.patch('pearllib.pearlenv.os') as os_mock:
        os_mock.environ = {
            'PEARL_ROOT': str(root),
        }
        return PearlEnvironment(home, config_filename=config_filename)


