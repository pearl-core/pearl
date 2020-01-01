import logging
import subprocess
import shutil
from pathlib import Path

import sys


class Messenger:
    def __init__(self):
        self.logger = logging.getLogger('message')
        out_hdlr = logging.StreamHandler(sys.stdout)
        out_hdlr.setFormatter(logging.Formatter('%(message)s'))
        out_hdlr.setLevel(logging.INFO)
        for handler in self.logger.handlers:
            self.logger.removeHandler(handler)
        self.logger.addHandler(out_hdlr)
        self.logger.setLevel(logging.INFO)

    def print(self, message):
        self.logger.info(message)

    def info(self, message):
        self.logger.info('{}{}{}'.format(Color.CYAN, message, Color.NORMAL))

    def warn(self, message):
        self.logger.warning('{}{}{}'.format(Color.YELLOW, message, Color.NORMAL))

    def error(self, message):
        self.logger.error('{}{}{}'.format(Color.RED, message, Color.NORMAL))


messenger = Messenger()


def verify_git_dep():
    git_version_min = 'git version 1.8.5'
    obj = run("git version", capture_stdout=True, check=False)
    git_version = obj.stdout.strip() if obj.stdout else None
    git_status = obj.returncode
    if git_status == 127:
        raise EnvironmentError('The command git has not been found. Exiting...')

    if git_version < git_version_min:
        messenger.warn(
            "Pearl might not work properly since git is too old: {} < {}".format(
                git_version, git_version_min
            )
        )
        return False

    return True


def verify_bash_dep():
    bash_version_min = "4.1"
    obj = run("echo $BASH_VERSION", capture_stdout=True, check=False)
    bash_version = obj.stdout.strip() if obj.stdout else None
    bash_status = obj.returncode
    if bash_status == 127:
        raise EnvironmentError('The command bash has not been found. Exiting...')
    if bash_version is None:
        messenger.warn(
            "Warn: The BASH_VERSION environment variable is not defined"
        )
        return False
    elif bash_version < bash_version_min:
        messenger.warn(
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


def run(script: str, capture_stdout=False, capture_stderr=False, check=True):
    return subprocess.run(
        ['/usr/bin/env', 'bash', '-c', script],
        check=check,
        stdout=subprocess.PIPE if capture_stdout else None,
        stderr=subprocess.PIPE if capture_stderr else None,
        universal_newlines=True
    )


class Color:
    RED = "\033[1;31m"
    CYAN = "\033[1;36m"
    YELLOW = "\033[1;33m"
    NORMAL = "\033[0m"
    PINK = "\033[1;35m"


def ask(prompt: str, yes_as_default_answer=False):
    if yes_as_default_answer:
        default_answer = "Y"
        other_answer = "n"
    else:
        default_answer = "N"
        other_answer = "y"

    answer = None
    while answer not in ['Y', 'N']:
        answer = input(messenger.info('{} ({}/{})'.format(prompt, default_answer, other_answer))).upper()
        if not answer:
            answer = default_answer

    return answer == "Y"


def apply(line: str, filename: str):
    path = Path(filename)
    path.parent.mkdir(parents=True, exist_ok=True)

    if not path.exists():
        path.touch()
    with path.open("r+") as f:
        content = f.read()
        if line not in content.split('\n'):
            f.seek(0)
            f.write("{}\n{}".format(line, content))


def unapply(line: str, filename: str):
    path = Path(filename)
    if not path.exists():
        return
    with path.open("r+") as f:
        writeable_content = f.read().replace(line + '\n', '').replace(line, "")
    with path.open("w") as f:
        f.write(writeable_content)
