from pathlib import Path

import pytest

from pearllib.parser import parse_args


def test_parser_valid():
    args = parse_args(['install', 'pkg1', 'pkg2'])
    assert args.command == 'install'
    assert args.packages == ['pkg1', 'pkg2']

    args = parse_args(['update', 'pkg1', 'pkg2'])
    assert args.command == 'update'
    assert args.packages == ['pkg1', 'pkg2']

    args = parse_args(['remove', 'pkg1', 'pkg2'])
    assert args.command == 'remove'
    assert args.packages == ['pkg1', 'pkg2']

    args = parse_args(['emerge', 'pkg1', 'pkg2'])
    assert args.command == 'emerge'
    assert args.packages == ['pkg1', 'pkg2']

    args = parse_args(['info', 'pkg1', 'pkg2'])
    assert args.command == 'info'
    assert args.packages == ['pkg1', 'pkg2']

    args = parse_args(['search', 'patt'])
    assert args.command == 'search'
    assert args.pattern == 'patt'
    assert not args.package_only

    args = parse_args(['search', '--package-only', 'patt'])
    assert args.command == 'search'
    assert args.pattern == 'patt'
    assert args.package_only

    args = parse_args(['list'])
    assert args.command == 'list'
    assert not args.package_only

    args = parse_args(['list', '--package-only'])
    assert args.command == 'list'
    assert args.package_only

    args = parse_args(['create', 'new-pkg', 'dest-dir'])
    assert args.command == 'create'
    assert args.name == 'new-pkg'
    assert isinstance(args.dest_dir, Path)
    assert str(args.dest_dir) == 'dest-dir'

    args = parse_args(['init'])
    assert args.command == 'init'


def test_parser_general_options():
    args = parse_args(['init'])
    assert not args.update_repos
    assert not args.no_confirm
    assert not args.force
    assert not args.config_file
    assert not args.verbose

    args = parse_args(['-u', '-f', '-vv', '-n', '-c', 'pearl.conf', 'init'])
    assert args.update_repos
    assert args.no_confirm
    assert args.force
    assert isinstance(args.config_file, Path)
    assert str(args.config_file) == 'pearl.conf'
    assert args.verbose == 2


def test_parser_no_command():
    with pytest.raises(SystemExit):
        parse_args(['-c', 'pearl.conf', '--verbose'])
