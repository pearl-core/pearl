import logging
import sys


class Color:
    RED = "\033[1;31m"
    CYAN = "\033[1;36m"
    YELLOW = "\033[1;33m"
    NORMAL = "\033[0m"
    PINK = "\033[1;35m"


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

    def exception(self, message):
        self.logger.exception('{}{}{}'.format(Color.RED, message, Color.NORMAL))


messenger = Messenger()
