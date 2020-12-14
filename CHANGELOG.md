# Change Log #

## [2.3.5][v235] - 2020-12-14 ##

* Source `config.*` files according to dependency tree order
* Deprecate support for Python 3.5
* Add `PEARL_DEBUG` environment variable
* Fix integ tests for OSX

## [2.3.4][v234] - 2020-06-28 ##

* Fix when URL change local/git package

## [2.3.3][v233] - 2020-02-20 ##

* Add FUNDING file
* Add new sections in README
* Add PyPi badges in README

## [2.3.2][v232] - 2020-02-15 ##

* Show traceback only on unexpected exception
  * To see traceback about pearl error, use the `-v` option
* Add ability to remove packages even when they do not exist in repository anymore

## [2.3.1][v231] - 2020-01-29 ##

* Fix bug when removing packages
* Refactor README.md

## [2.3.0][v230] - 2020-01-25 ##

* Handle dependencies between packages
  * During `install`, `update` and `emerge` packages dependencies will be included as well.
  * During `remove` only packages not required by others can be removed.
* Include `Required by` in `info` command

## [2.2.0][v220] - 2020-01-17 ##

* Add `info` command
* Make `search` looking at keywords field

## [2.1.2][v212] - 2020-01-14 ##

* Change name PyPI package from `pearcli` to `pearl`
* Add `package.conf`

## [2.1.1][v211] - 2020-01-13 ##

* Replace `install.sh` with `hooks.sh`
  * `install.sh` will still be valid until next releases
* Fail if no command is specified

## [2.1.0][v210] - 2020-01-12 ##

* Add `create` command
* Fix procedure to install Pearl in OSX
* Fix ci to upload to PyPI

## [2.0.2][v202] - 2020-01-11 ##

* Add instructions to install Pearl in Arch Linux
* Fix bug for version option

## [2.0.1][v201] - 2020-01-11 ##

* Manual intervention to switch to Pearl v2
  * [Migration page](https://github.com/pearl-core/pearl/wiki/Migration-to-Pearl-version-2)
* Codebase re-written in Python
* Remove the `post_remove`
* `pearl.conf` is not a python script. This requires manual intervention. Take a look at the `pearl.conf.template` file in codebase
* `pearl-metadata` directory is finally deprecated
* Add `--no-confirm` option
* Add `--force` option
  * This option bypasses failures even during the hook function execution
* Add `--verbose` option
  * `-vv` allows to enable xtrace in hook functions
* Add `--update-repos` option
* Shortcut commands (i.e. `i` to specify `install` command) are no longer available
* Pearl file locations change drastically in order to be complaint with the
[XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
  * As of now, `pearl.conf` resides in `$XDG_CONFIG_HOME/pearl` (default `~/.config/pearl`)
  * The new location for `$PEARL_HOME` is `$XDG_DATA_HOME/pearl` (default `~/.local/share/pearl`)
* Remove the variables `$PEARL_ROOT` and `$PEARL_TEMPORARY`

## [1.8.2][v182] - 2019-10-13 ##

* Update `README.md` file

## [1.8.1][v181] - 2019-07-13 ##

* Update `buava`:
  * New view action for `setup_configuration` helper function

## [1.8.0][v180] - 2019-06-03 ##

* Add roll back mechanism during install package
* Add `grep` and `sed` as optional dependencies since they may be used in hook functions
* Add newer `buava`:
  * `backup` function
  * `delete` function
  * `ideavim` and `gvim` dotfiles for (`un`)`link` functions
  * `install_or_update_vim_plugin_git_repo` function
  *  Add GNU `sed` and `grep` for OSX compat functions

## [1.7.2][v172] - 2019-01-15 ##

* Fix variables `PEARL_PKGNAME` `PEARL_PKGREPONAME` for vim and emacs boot

## [1.7.1][v171] - 2018-08-11 ##

* Deprecate pathogen
* Ensure to `cd` when updating package pointing to local directory
* Fix when package specified with full name does not exist
* More log info when Git URL package change
* Proceed even if `install.sh` is syntatically incorrect (prevent block for fixing the broken package)
* Improve doc and add section about comparison with Ansible

## [1.7.0][v170] - 2018-07-05 ##

* Add the idempotent `emerge` command which update/install packages.
* No longer support the use of USR1 signal to source the Pearl config to the parent process. To explictly do that run `pearl-source` command instead.

## [1.6.3][v163] - 2018-06-22 ##

* Location of the repo file is `pearl-config/pearl.conf`. Backward compatibility will be kept until 2.0.0
* Make the branch name inferred from repo's HEAD rather than hardcode the branch with `master`

## [1.6.2][v162] - 2018-06-09 ##

* Add variables `PEARL_PKGNAME` `PEARL_PKGREPONAME`
* Fix boot vim for deprecating `pearl-metadata`
* Update doc to use dynamic updates for third-party git repos
* Add the buava git repo helpers

## [1.6.1][v161] - 2018-02-04 ##

* Fix import `osx-compat.sh`
* Fix update Pearl submodules during updates

## [1.6.0][v160] - 2018-02-03 ##

* Change directory name to `pearl-config`. Pearl version `2.0.0` will deprecate `pearl-metadata`
* `link_to_path` to customize symlink name
* Changes in `buava` for Pearl configs:
  * `osx_detect` function to detect the OS platform
  * Improved `choose` function with indexes
  * Add `ssh` for `[un]link` function

## [1.5.6][v156] - 2017-08-31 ##

* Fix Integ tests

## [1.5.5][v155] - 2017-08-31 ##

* Update Buava:
  * Update `download` function
  * Add `choose`, `input` and `contain_elements` functions

## [1.5.4][v154] - 2017-08-28 ##

* Update Buava:
  * Add `download` function
  * vimperator gtk2 programs for `[un]link` functions

## [1.5.3][v153] - 2017-06-29 ##

* Add [Pear test utils](https://github.com/pearl-core/test-utils) as new dependency
* Add [Bunit](https://github.com/fsquillace/bunit) as new dependency
* Add [Buava](https://github.com/fsquillace/buava) as new dependency
* Inform about the trap on USR1 signal

## [1.5.2][v152] - 2017-01-07 ##

* Add support for new OSX image in Travis
* Fallback to a default temp directory if `tty` does not work

## [1.5.1][v151] - 2016-11-15 ##

* Fix git --no-parser log for missing newline

## [1.5.0][v150] - 2016-11-13 ##

* Provide (un)link from/to in utils.sh
* Provide list of last commits during add/update package
* Fix `unlink_from_path` when source file is a symlink

## [1.4.5][v145] - 2016-11-11 ##

* Provide (un)link from/to PATH variable in utils.sh

## [1.4.4][v144] - 2016-09-26 ##

* Improving doc and add checkstyle

## [1.4.3][v143] - 2016-05-25 ##

* Remove the requirement of updating the PATH on OSX

## [1.4.2][v142] - 2016-05-10 ##

* Add support for OSX
* Add check for existing `PEARL_HOME` variable for emacs/vim boot scripts
* Add `PEARL_HOME/bin` directory to have symlinks for the Pearl packages executables
* Avoid polluting `PATH` variable by introducing a check first

## [1.4.1][v141] - 2016-04-30 ##

* Introduce `$PEARL_PKGVARDIR` on boot scripts
* Packages do not need to have `master` as default branch
* Change the installation process to avoid [pipe bash problem](https://www.idontplaydarts.com/2016/04/detecting-curl-pipe-bash-server-side/)
* Ensure to get the most updated `post_update` function
* Refactor unit tests in `test-package.sh`

## [1.4.0][v140] - 2016-04-23 ##

* Add a dedicated directory `$PEARL_PKGVARDIR` for the Pearl packages in order to store data
  needed during the execution of the package itself
* Add warning in case of an old version of git or bash
* Introduce `$PEARL_PKGDIR` environment variable for emacs and vim config files
* Change the definition of public API

## [1.3.1][v131] - 2016-04-21 ##

* Fix compatibility with Bash 4.1
* Integration tests with fixed Bash and Git versions

## [1.3.0][v130] - 2016-04-20 ##

* Provide the definition of public API
* Add the emacs hook
* Use a better approach to return values from bash functions
* Change location of the boot files for pearl.fish and pearl.sh
* Introduce the standard documentation for functions
* Use try/catch approach to handle errors

## [1.2.0][v120] - 2016-04-14 ##

* Fix the removal of packages by querying the local directory
* Add (un)link functions for utils.sh

## [1.1.0][v110] - 2016-04-09 ##

* Check if Git URL changed during updates
* Introduce the template for new Pearl packages
* Local directories can be used as Pearl packages
* Add VERSION file

## [1.0.1][v101] - 2016-04-08 ##

* Update docs
* Add travis and integration tests
* Introduce the installer

## [1.0.0][v100] - 2016-04-03 ##

* Initial commit.

<!--  Links -->

[v100]: https://github.com/pearl-core/pearl/releases/tag/1.0.0
[v101]: https://github.com/pearl-core/pearl/releases/tag/1.0.1
[v110]: https://github.com/pearl-core/pearl/releases/tag/1.1.0
[v120]: https://github.com/pearl-core/pearl/releases/tag/1.2.0
[v130]: https://github.com/pearl-core/pearl/releases/tag/1.3.0
[v131]: https://github.com/pearl-core/pearl/releases/tag/1.3.1
[v140]: https://github.com/pearl-core/pearl/releases/tag/1.4.0
[v141]: https://github.com/pearl-core/pearl/releases/tag/1.4.1
[v142]: https://github.com/pearl-core/pearl/releases/tag/1.4.2
[v143]: https://github.com/pearl-core/pearl/releases/tag/1.4.3
[v144]: https://github.com/pearl-core/pearl/releases/tag/1.4.4
[v145]: https://github.com/pearl-core/pearl/releases/tag/1.4.5
[v150]: https://github.com/pearl-core/pearl/releases/tag/1.5.0
[v151]: https://github.com/pearl-core/pearl/releases/tag/1.5.1
[v152]: https://github.com/pearl-core/pearl/releases/tag/1.5.2
[v153]: https://github.com/pearl-core/pearl/releases/tag/1.5.3
[v154]: https://github.com/pearl-core/pearl/releases/tag/1.5.4
[v155]: https://github.com/pearl-core/pearl/releases/tag/1.5.5
[v156]: https://github.com/pearl-core/pearl/releases/tag/1.5.6
[v160]: https://github.com/pearl-core/pearl/releases/tag/1.6.0
[v161]: https://github.com/pearl-core/pearl/releases/tag/1.6.1
[v162]: https://github.com/pearl-core/pearl/releases/tag/1.6.2
[v163]: https://github.com/pearl-core/pearl/releases/tag/1.6.3
[v170]: https://github.com/pearl-core/pearl/releases/tag/1.7.0
[v171]: https://github.com/pearl-core/pearl/releases/tag/1.7.1
[v172]: https://github.com/pearl-core/pearl/releases/tag/1.7.2
[v180]: https://github.com/pearl-core/pearl/releases/tag/1.8.0
[v181]: https://github.com/pearl-core/pearl/releases/tag/1.8.1
[v182]: https://github.com/pearl-core/pearl/releases/tag/1.8.2
[v201]: https://github.com/pearl-core/pearl/releases/tag/2.0.1
[v202]: https://github.com/pearl-core/pearl/releases/tag/2.0.2
[v210]: https://github.com/pearl-core/pearl/releases/tag/2.1.0
[v211]: https://github.com/pearl-core/pearl/releases/tag/2.1.1
[v212]: https://github.com/pearl-core/pearl/releases/tag/2.1.2
[v220]: https://github.com/pearl-core/pearl/releases/tag/2.2.0
[v230]: https://github.com/pearl-core/pearl/releases/tag/2.3.0
[v231]: https://github.com/pearl-core/pearl/releases/tag/2.3.1
[v232]: https://github.com/pearl-core/pearl/releases/tag/2.3.2
[v233]: https://github.com/pearl-core/pearl/releases/tag/2.3.3
[v234]: https://github.com/pearl-core/pearl/releases/tag/2.3.4
[v235]: https://github.com/pearl-core/pearl/releases/tag/2.3.5
