import argparse
import textwrap
from pathlib import Path

import pearllib.package as pack
import pearllib.system as syst
from pearllib.exceptions import PearlError

from pearllib.pearlenv import PearlEnvironment
from pearllib.utils import verify_runtime_deps, messenger


def _parse_args(sys_args: list):
    parser = _create_main_parser()

    command_parsers = parser.add_subparsers(
        help='For command help: %(prog)s COMMAND --help',
        title='commands',
        dest='subparser_name'
    )

    _create_install_parser(command_parsers)
    _create_update_parser(command_parsers)
    _create_remove_parser(command_parsers)
    _create_emerge_parser(command_parsers)
    _create_init_parser(command_parsers)
    _create_search_parser(command_parsers)
    _create_list_parser(command_parsers)

    args = parser.parse_args(sys_args)
    return args


def _create_main_parser():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent("""
        Pearl: Because only in the best shells you will find a Pearl...
        """))
    parser.add_argument(
        '-u', '--update-repos',
        action='store_true',
        help="update the repositories before doing any action"
    )
    parser.add_argument(
        '-n', '--no-confirm',
        action='store_true',
        help="Bypass all “Are you sure?” messages."
    )
    parser.add_argument(
        '-c', '--config-file', metavar='FILE', type=str,
        default=None,
        help="location of the pearl config path. Defaults to $HOME/.config/pearl/pearl.conf"
    )
    parser.add_argument('--verbose', '-v', action='count', default=0, help="increase output verbosity")
    parser.add_argument('--version', '-V', action='version', version='%(prog)s 2.0')
    return parser


def _create_install_parser(command_parsers):
    install_parser = command_parsers.add_parser(
        'install',
        help='Install the packages'
    )
    install_parser.add_argument(
        'packages', metavar='[REPO/]PACKAGE', type=str, nargs='+'
    )


def _create_update_parser(command_parsers):
    update_parser = command_parsers.add_parser(
        'update',
        help='Update Pearl or the packages if specified'
    )
    update_parser.add_argument(
        'packages', metavar='[REPO/]PACKAGE', type=str, nargs='*'
    )


def _create_remove_parser(command_parsers):
    remove_parser = command_parsers.add_parser(
        'remove',
        help='Remove Pearl or the packages if specified'
    )
    remove_parser.add_argument(
        'packages', metavar='[repo/]package', type=str, nargs='*'
    )


def _create_emerge_parser(command_parsers):
    emerge_parser = command_parsers.add_parser(
        'emerge',
        help='Update Pearl or install/update the packages if specified'
    )
    emerge_parser.add_argument(
        'packages', metavar='[repo/]package', type=str, nargs='*'
    )


def _create_init_parser(command_parsers):
    command_parsers.add_parser(
        'init',
        help='Init $HOME/.config/pearl config directory'
    )


def _create_search_parser(command_parsers):
    command_parser = command_parsers.add_parser(
        'search',
        help='Search the available Pearl packages that match pattern'
    )
    command_parser.add_argument(
        'pattern', metavar='PATTERN', type=str
    )


def _create_list_parser(command_parsers):
    command_parsers.add_parser(
        'list',
        help='List all the available Pearl packages'
    )


def _package_operation(packages):
    captured_exception = None
    for package in packages:
        try:
            yield package
        except PearlError as ex:
            captured_exception = ex
    if captured_exception:
        raise captured_exception


def _pearl(pearl_env, args):
    if args.subparser_name == 'init':
        syst.init_pearl(pearl_env)
    elif args.subparser_name == 'install':
        for package in _package_operation(args.packages):
            pack.install_package(pearl_env, package, no_confirm=args.no_confirm)
    elif args.subparser_name == 'update':
        if not args.packages:
            syst.update_pearl(pearl_env, no_confirm=args.no_confirm)
        else:
            for package in _package_operation(args.packages):
                pack.update_package(pearl_env, package, no_confirm=args.no_confirm)
    elif args.subparser_name == 'remove':
        if not args.packages:
            syst.remove_pearl(pearl_env, no_confirm=args.no_confirm)
        else:
            for package in _package_operation(args.packages):
                pack.remove_package(pearl_env, package, no_confirm=args.no_confirm)
    elif args.subparser_name == 'emerge':
        for package in _package_operation(args.packages):
            pack.emerge_package(pearl_env, package, no_confirm=args.no_confirm)
    elif args.subparser_name == 'list':
        pack.list_packages(pearl_env)
    elif args.subparser_name == 'search':
        pack.list_packages(pearl_env, args.pattern)


def pearl(sys_args: list, pearl_root_dir: Path = None, pearl_home_dir: Path = None):
    verify_runtime_deps()

    args = _parse_args(sys_args)

    pearl_env = PearlEnvironment(
        home=pearl_home_dir,
        root=pearl_root_dir,
        config_filename=Path(args.config_file) if args.config_file is not None else None,
        update_repos=args.update_repos
    )

    try:
        _pearl(pearl_env, args)
    except PearlError as ex:
        messenger.error('Pearl error: '.format(ex.args))
        exit(ex.exit_status)

