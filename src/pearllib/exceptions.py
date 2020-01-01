
class PearlError(RuntimeError):
    def __init__(self, message, exit_status):
        super().__init__(message)
        self.exit_status = exit_status


class RepoDoesNotExistError(PearlError):
    def __init__(self, message):
        super().__init__(message, 105)


class PearlNotInRepoError(PearlError):
    def __init__(self, message):
        super().__init__(message, 104)


class PearlAlreadyInstalledError(PearlError):
    def __init__(self, message):
        super().__init__(message, 102)


class PearlNotInstalledError(PearlError):
    def __init__(self, message):
        super().__init__(message, 103)
