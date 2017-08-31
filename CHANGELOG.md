# Change Log #

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
