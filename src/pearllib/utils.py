import logging
import subprocess
import shutil
from pathlib import Path

logger = logging.getLogger(__name__)


def verify_git_dep():
    git_version_min = 'git version 1.8.5'
    git_status, git_version = subprocess.getstatusoutput('git version')
    if git_status == 127:
        raise EnvironmentError('The command git has not been found. Exiting...')

    if git_version < git_version_min:
        logger.warning(
            "Warn: Pearl might not work properly since git is too old: {} < {}".format(
                git_version, git_version_min
            )
        )
        return False

    return True


def verify_bash_dep():
    bash_version_min = "4.1"
    bash_status, bash_version = subprocess.getstatusoutput('bash -c "echo $BASH_VERSION"')
    if bash_status == 127:
        raise EnvironmentError('The command bash has not been found. Exiting...')
    if bash_version is None:
        logger.warning(
            "Warn: The BASH_VERSION environment variable is not defined"
        )
        return False
    elif bash_version < bash_version_min:
        logger.warning(
            "Warn: Pearl might not work properly since bash is too old: {} < {}".format(
                bash_version, bash_version_min
            )
        )
        return False
    return True


def verify_runtime_deps():
    verify_git_dep()
    verify_bash_dep()


def check_and_copy(src_dir: Path, dst_dir: Path):
    if not src_dir.is_dir():
        raise NotADirectoryError('{} is not a directory'.format(src_dir))
    shutil.rmtree(str(dst_dir))
    shutil.copytree(str(src_dir), str(dst_dir))


def run(script):
    subprocess.run(
        # TODO be careful to the bash location path
        ['/usr/local/bin/bash', '-c', script],
        check=True,
        universal_newlines=True
    )
