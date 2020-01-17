from pathlib import Path

import pearllib.package as pack
import pearllib.system as syst
from pearllib.exceptions import PearlError
from pearllib.messenger import messenger
from pearllib.parser import parse_args

from pearllib.pearlenv import PearlEnvironment
from pearllib.utils import verify_runtime_deps


def _package_operation(pearl_env, args, pack_func):
    captured_exception = None
    for package in args.packages:
        try:
            pack_func(pearl_env, package, args)
        except PearlError as ex:
            captured_exception = ex
    if captured_exception:
        raise captured_exception


def _pearl(pearl_env: PearlEnvironment, args):
    command = args.command
    if command == 'init':
        syst.init_pearl(pearl_env, args)
    elif command == 'install':
        _package_operation(pearl_env, args, pack.install_package)
    elif command == 'update':
        if not args.packages:
            syst.update_pearl(pearl_env, args)
        else:
            _package_operation(pearl_env, args, pack.update_package)
    elif command == 'remove':
        if not args.packages:
            syst.remove_pearl(pearl_env, args)
        else:
            _package_operation(pearl_env, args, pack.remove_package)
    elif command == 'emerge':
        _package_operation(pearl_env, args, pack.emerge_package)
    elif command == 'info':
        _package_operation(pearl_env, args, pack.info_package)
    elif command == 'list':
        pack.list_packages(pearl_env, args)
    elif command == 'search':
        pack.list_packages(pearl_env, args)
    elif command == 'create':
        pack.create_package(pearl_env, args)
    else:
        raise ValueError('No command specified. Run "pearl --help" for list of commands.')


def pearl(sys_args: list, pearl_home_dir: Path = None):
    verify_runtime_deps()

    args = parse_args(sys_args)
    if args.verbose:
        messenger.enable_debug()

    messenger.debug(args)

    pearl_env = PearlEnvironment(
        home=pearl_home_dir,
        config_filename=args.config_file,
        update_repos=args.update_repos,
        verbose=args.verbose,
        env_initialized=False if args.command == 'init' else True
    )

    try:
        _pearl(pearl_env, args)
    except PearlError as ex:
        messenger.exception('Pearl error: {}'.format(ex.args))
        exit(ex.exit_status)
