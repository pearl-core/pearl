import argparse
from pathlib import Path

import pkg_resources


def parse_args(sys_args: list):
    parser = _create_main_parser()

    command_parsers = parser.add_subparsers(
        help='For command help: %(prog)s COMMAND --help',
        title='commands',
        dest='command',
    )
    command_parsers.required = True

    _create_install_parser(command_parsers)
    _create_update_parser(command_parsers)
    _create_remove_parser(command_parsers)
    _create_emerge_parser(command_parsers)
    _create_info_parser(command_parsers)
    _create_init_parser(command_parsers)
    _create_search_parser(command_parsers)
    _create_list_parser(command_parsers)
    _create_create_parser(command_parsers)

    args = parser.parse_args(sys_args)
    return args


def _create_main_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-u', '--update-repos',
        action='store_true',
        help="update the repositories before doing any action"
    )
    parser.add_argument(
        '-n', '--no-confirm',
        action='store_true',
        help="Bypass all “Are you sure?” messages"
    )
    parser.add_argument(
        '-f', '--force',
        action='store_true',
        help="Continue performing the action even if hooks functions fail"
    )
    parser.add_argument(
        '-c', '--config-file', metavar='FILE', type=Path,
        default=None,
        help="location of the pearl config path. Defaults to $HOME/.config/pearl/pearl.conf"
    )
    parser.add_argument(
        '--verbose', '-v', action='count', default=0,
        help="-v increases output verbosity. "
             "-vv shows bash xtrace during the hook function execution."
    )
    version = pkg_resources.require("pearl")[0].version
    parser.add_argument('--version', '-V', action='version', version='%(prog)s {}'.format(version))
    return parser


def _create_install_parser(command_parsers):
    parser = command_parsers.add_parser(
        'install',
        help='Install the packages'
    )
    parser.add_argument(
        'packages', metavar='[REPO/]PACKAGE', type=str, nargs='+'
    )


def _create_update_parser(command_parsers):
    parser = command_parsers.add_parser(
        'update',
        help='Update Pearl or the packages if specified'
    )
    parser.add_argument(
        'packages', metavar='[REPO/]PACKAGE', type=str, nargs='*'
    )


def _create_remove_parser(command_parsers):
    parser = command_parsers.add_parser(
        'remove',
        help='Remove Pearl or the packages if specified'
    )
    parser.add_argument(
        'packages', metavar='[repo/]package', type=str, nargs='*'
    )


def _create_emerge_parser(command_parsers):
    parser = command_parsers.add_parser(
        'emerge',
        help='Update Pearl or install/update the packages if specified'
    )
    parser.add_argument(
        'packages', metavar='[repo/]package', type=str, nargs='*'
    )


def _create_info_parser(command_parsers):
    parser = command_parsers.add_parser(
        'info',
        help='Provide information about packages'
    )
    parser.add_argument(
        'packages', metavar='[repo/]package', type=str, nargs='*'
    )


def _create_init_parser(command_parsers):
    command_parsers.add_parser(
        'init',
        help='Init Pearl directories and files: $HOME/.local/share/pearl and $HOME/.config/pearl/pearl.conf'
    )


def _create_search_parser(command_parsers):
    parser = command_parsers.add_parser(
        'search',
        help='Search the available Pearl packages that match pattern'
    )
    parser.add_argument(
        'pattern', metavar='PATTERN', type=str,
        default=".*"
    )
    parser.add_argument(
        '-p', '--package-only',
        action='store_true',
        help="List the package names only"
    )


def _create_list_parser(command_parsers):
    parser = command_parsers.add_parser(
        'list',
        help='List all the available Pearl packages'
    )
    parser.add_argument(
        '-p', '--package-only',
        action='store_true',
        help="List the package names only"
    )


def _create_create_parser(command_parsers):
    parser = command_parsers.add_parser(
        'create',
        help='Create a new local Pearl package to a new or already existing directory'
    )
    parser.add_argument(
        'name', metavar='NAME', type=str,
        help="Name of the local package"
    )
    parser.add_argument(
        'dest_dir', metavar='DEST_DIR', type=Path,
        help="Directory path where the 'pearl-config' directory template will be placed"
    )
