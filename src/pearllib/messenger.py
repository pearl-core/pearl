import logging
import sys


class Color:
    RED = "\033[0;31m"
    GREEN = "\033[0;32m"
    CYAN = "\033[0;36m"
    YELLOW = "\033[0;33m"
    NORMAL = "\033[0m"
    PINK = "\033[0;35m"


class Messenger:
    def __init__(self):
        self.logger = logging.getLogger('message')
        self._init_logger()

    def _init_logger(self, debug=False):
        out_hdlr = logging.StreamHandler(sys.stdout)
        out_hdlr.setFormatter(logging.Formatter('%(message)s'))

        log_level = logging.DEBUG if debug else logging.INFO
        out_hdlr.setLevel(log_level)

        for handler in self.logger.handlers:
            self.logger.removeHandler(handler)
        self.logger.addHandler(out_hdlr)

        self.logger.setLevel(log_level)

    def enable_debug(self):
        self._init_logger(True)

    def print(self, message):
        self.logger.info(message)

    def debug(self, message):
        self.logger.debug('{}{}{}'.format(Color.GREEN, message, Color.NORMAL))

    def info(self, message):
        self.logger.info('{}{}{}'.format(Color.CYAN, message, Color.NORMAL))

    def warn(self, message):
        self.logger.warning('{}{}{}'.format(Color.YELLOW, message, Color.NORMAL))

    def error(self, message):
        self.logger.error('{}{}{}'.format(Color.RED, message, Color.NORMAL))

    def exception(self, message):
        self.logger.exception('{}{}{}'.format(Color.RED, message, Color.NORMAL))


messenger = Messenger()
