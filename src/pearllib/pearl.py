from enum import Enum
from pathlib import Path
from typing import List

import pearllib.package as pack
import pearllib.system as syst
from pearllib.exceptions import PearlError, RepoDoesNotExistError, PackageNotInRepoError
from pearllib.messenger import messenger
from pearllib.parser import parse_args

from pearllib.pearlenv import PearlEnvironment, Package
from pearllib.utils import verify_runtime_deps


class PearlCommand(Enum):
    INIT = 'init'
    INSTALL = 'install'
    UPDATE = 'update'
    INFO = 'info'
    CREATE = 'create'
    EMERGE = 'emerge'
    LIST = 'list'
    SEARCH = 'search'
    REMOVE = 'remove'

    @staticmethod
    def from_string(command: str) -> 'PearlCommand':
        return PearlCommand.__members__[command.upper()]

    def __str__(self):
        return self.name


def _extract_packages(pearl_env: PearlEnvironment, command: PearlCommand, args) -> List[Package]:
    try:
        return [pearl_env.lookup_package(package_name) for package_name in args.packages]
    except (RepoDoesNotExistError, PackageNotInRepoError):
        if command != PearlCommand.REMOVE:
            raise

        if args.force:
            # The user can still be able to remove a package even if package does not exist in any repository.
            return [pearl_env.infer_package(package_name) for package_name in args.packages]
        else:
            messenger.info("To remove the package, use option --force.")
            raise


def _pearl(pearl_env: PearlEnvironment, args):
    command = PearlCommand.from_string(args.command)
    if hasattr(args, 'packages'):
        args.packages = _extract_packages(pearl_env, command, args)

    if command == PearlCommand.INIT:
        syst.init_pearl(pearl_env, args)
    elif command == PearlCommand.INSTALL:
        pack.install_packages(pearl_env, args)
    elif command == PearlCommand.UPDATE:
        if not args.packages:
            syst.update_pearl(pearl_env, args)
        else:
            pack.update_packages(pearl_env, args)
    elif command == PearlCommand.REMOVE:
        if not args.packages:
            syst.remove_pearl(pearl_env, args)
        else:
            pack.remove_packages(pearl_env, args)
    elif command == PearlCommand.EMERGE:
        pack.emerge_packages(pearl_env, args)
    elif command == PearlCommand.INFO:
        pack.info_packages(pearl_env, args)
    elif command == PearlCommand.LIST:
        pack.list_packages(pearl_env, args)
    elif command == PearlCommand.SEARCH:
        pack.list_packages(pearl_env, args)
    elif command == PearlCommand.CREATE:
        pack.create_package(pearl_env, args)
    else:
        raise ValueError('No command specified. Run "pearl --help" for list of commands.')


def pearl(sys_args: list, pearl_home_dir: Path = None):
    verify_runtime_deps()

    args = parse_args(sys_args)
    if args.verbose:
        messenger.enable_debug()

    messenger.debug(args)

    try:
        pearl_env = PearlEnvironment(
            home=pearl_home_dir,
            config_filename=args.config_file,
            update_repos=args.update_repos,
            verbose=args.verbose,
            env_initialized=False if args.command == 'init' else True
        )

        _pearl(pearl_env, args)
    except PearlError as ex:
        message = ex.args[0] if ex.args else None
        if args.verbose:
            messenger.exception('Pearl error: {}'.format(message))
        else:
            messenger.error(message)
        exit(ex.exit_status)
