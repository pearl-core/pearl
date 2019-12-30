
class RepoDoesNotExistError(RuntimeError):
    pass


class PackageNotInRepoError(RuntimeError):
    pass


class PackageAlreadyInstalledError(RuntimeError):
    pass


class PackageNotInstalledError(RuntimeError):
    pass
