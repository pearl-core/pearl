Pearl
=====

Because only in the best Shells you will find a Pearl...

```sh
wget https://raw.githubusercontent.com/pearl-core/installer/master/install.sh
bash install.sh
```

<h1 align="center">
    <a href="https://github.com/pearl-core/pearl"><img
        alt="Pearl"
        width=300px
        src="https://rawgit.com/pearl-core/logo/master/pearl.png"></a>
</h1>

|Project Status|Communication|
|:-----------:|:-----------:|
|[![Build status](https://api.travis-ci.org/pearl-core/pearl.png?branch=master)](https://travis-ci.org/pearl-core/pearl) | [![Join the gitter chat at https://gitter.im/pearl-core/pearl](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/pearl-core/pearl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) |

**Table of Contents**
- [Description](#description)
- [Quickstart](#quickstart)
- [Installation](#installation)
  - [Dependencies](#dependencies)
  - [Linux](#linux)
  - [OSX](#osx)
- [Create your own Pearl package in seconds!](#create-your-own-pearl-package-in-seconds)
- [Create your own Pearl repository in seconds!](#create-your-own-pearl-repository-in-seconds)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)

Description
===========
**Pearl** is a package manager for dotfiles, plugins, programs
and any form of code accessible via git for Linux and OSX.

As soon as a package gets installed, its content can be activated out of the box
according to certain events, like, for instance, a shell startup (Bash, Zsh or Fish) or
an editor startup (Vim or Emacs). This is possible via a smart and simple
[hook mechanism](#create-your-own-pearl-package-in-seconds)
that integrates the package content within the Pearl ecosystem.

The main advantages on using Pearl are:

- Create your own Pearl package ***in seconds*** (any git repository is already a Pearl package)!
- Full control and sync of your dotfiles across different systems.
- Automatic bootstrap of the package content whenever shells or editors get started.
- Access to a wide range of existing packages via the [OPH (Official Pearl Hub)](https://github.com/pearl-hub).
- Allows to create your own package repository that can be shared with your friends!
- Stable codebase with 100+ unit tests and exhaustive integration tests via [Travis](https://travis-ci.org/pearl-core/pearl) for Linux and OSX.
- Small number of [dependencies](#dependencies) needed in order to ensure compatibility with most of the systems.

Quickstart
==========
The Pearl CLI script allows to: *list*, *search*, *install*, *update*,
*remove* the Pearl packages defined according to the configuration located in *~/.config/pearl/pearl.conf*

![quickstart](https://raw.githubusercontent.com/pearl-core/resources/master/pearl-opt3.gif)

List
----
- List all the available packages:

```sh
$ pearl list
...
pearl/dot-git
    Awesome git dotfiles (https://github.com/pearl-hub/git)
pearl/sesaila [installed]
    Awesome aliases for Bash, Zsh and Fish shells (https://github.com/pearl-hub/sesaila)
pearl/airline [installed]
    Status/tabline for vim (https://github.com/vim-airline/vim-airline)
pearl/trash [installed]
    Smart command to recover files you regretted to delete (https://github.com/pearl-hub/trash)
...
```

Search
------
- Search for `vim` Pearl packages:

```sh
$ pearl search vim
* Updating https://github.com/pearl-hub/repo.git repository
pearl/dot-vim
    Awesome vim dotfiles (https://github.com/pearl-hub/vim)
```

Install
-------
- Install `pearl/dot-vim` package (as soon as the package is installed the package is ready out of the box in vim editor!):

```sh
$ pearl install dot-vim
* Updating https://github.com/pearl-hub/repo.git repository
* Installing pearl/dot-vim package
```

- Install `pearl/trash` package:

```sh
$ pearl install trash
* Updating https://github.com/pearl-hub/repo.git repository
* Installing pearl/trash package
$ trash -h
Usage: trash file1 file2 ....
Moves to trash the files
Options:
        -s, --show                  Shows the trash
        -e, --empty                 Empties the trash
        -r, --recovery <file ...>   Recovers trashed files
        -c, --count                 Count the trashed files
        -h, --help                  Show this help message

```

Update
-------
- Update `pearl/dot-vim` package:

```sh
$ pearl update dot-vim
* Updating https://github.com/pearl-hub/repo.git repository
* Updating pearl/dot-vim package
```

- Update Pearl and all its packages installed:

```sh
$ pearl update
...
* Updating https://github.com/pearl-hub/repo.git repository
* Updating Pearl script
* Updating pearl/dot-vim package
* Updating pearl/airline package
* Updating pearl/trash package
* Updating pearl/caprica package
...
```

Remove
-------
- Remove `pearl/dot-vim` package:

```sh
$ pearl remove dot-vim
* Updating https://github.com/pearl-hub/repo.git repository
* Removing pearl/dot-vim package
```

- Remove Pearl and all its packages installed:

```sh
$ pearl remove
...
Are you sure to REMOVE all the Pearl packages in $PEARL_HOME folder? (N/y)
* Updating https://github.com/pearl-hub/repo.git repository
* Removing pearl/dot-vim package
* Removing pearl/airline package
* Removing pearl/trash package
* Removing pearl/caprica package
...
```

Recommended Pearl Hub packages to install:
-------------------------------

- [cmd](https://github.com/pearl-hub/cmd)
- [kyrat](https://github.com/pearl-hub/kyrat)
- [ranger](https://github.com/pearl-hub/ranger)
- [sesaila](https://github.com/pearl-hub/sesaila)
- [trash](https://github.com/pearl-hub/trash)
- [txum](https://github.com/pearl-hub/txum)

For dotfiles packages take a look [here](https://github.com/pearl-hub?q=dot).

Check out the [OPH (Official Pearl Hub)](https://github.com/pearl-hub)
for more packages you might be interested.

Installation
============

Dependencies
------------
Before installing Pearl be sure that all dependencies are properly installed in your system.
The Pearl dependencies are the following:

- [bash (>=4.1)](https://www.gnu.org/software/bash/)
- [git (>=1.8)](https://git-scm.com/)
- [GNU coreutils](https://www.gnu.org/software/coreutils/)
- [grep](https://www.gnu.org/software/grep/)

The following are ***optional*** dependencies in case you are using a different shell from
`bash`:

- [fish (>=2.2.0)](https://fishshell.com/)
- [zsh (>=5.2)](http://www.zsh.org/)

Linux
-----
Assuming all Pearl dependencies are properly installed in the system, to install Pearl
run the following:
```sh
wget https://raw.githubusercontent.com/pearl-core/installer/master/install.sh
# or
curl -LO  https://raw.githubusercontent.com/pearl-core/installer/master/install.sh
bash install.sh
```

OSX
---
In order to install all Pearl dependencies, you first need to install [Homebrew](http://brew.sh/).

To install all the needed dependencies via Homebrew:
```sh
brew update
brew install bash git coreutils
```

Once all Pearl dependencies are properly installed in the system, to install Pearl
run the following:
```sh
wget https://raw.githubusercontent.com/pearl-core/installer/master/install.sh
# or
curl -LO  https://raw.githubusercontent.com/pearl-core/installer/master/install.sh
bash install.sh
```

Create your own Pearl package in seconds!
===============
**Any git repository is already a Pearl package**. For instance, in order
to see a dotfiles repository in Pearl, you just need to change
the Pearl configuration file located in *$HOME/.config/pearl/pearl.conf*.

Add the following line to *pearl.conf* file:

    PEARL_PACKAGES["joe-dotfiles"]="https://github.com/joe/mydotfiles.git"

In other words, update the `PEARL_PACKAGES` array with a new entry containing the
name of the package (i.e. *joe-dotfiles*) and the git url (i.e. *https://github.com/joe/mydotfiles.git*).

***That's it!*** The package will be ready to be [installed](#install),
[updated](#update) and [removed](#remove) via the Pearl system.

Also, an optional description of the package can be defined via `PEARL_PACKAGES_DESCR` array:

    PEARL_PACKAGES_DESCR["joe-dotfiles"]="The Joe's dotfiles"

## Structure of a Pearl package ##
Your own git repository can contain an **optional** directory
named *pearl-config* used by Pearl to integrate the package with the Pearl environment.

    / (package root)
    │
    ├── pearl-config (optional directory)
    │   │
    │   ├── install.sh
    │   ├── config.sh
    │   ├── config.bash
    │   ├── config.zsh
    │   ├── config.fish
    │   ├── config.vim
    │   └── config.el
    │
    └── (additional package content)

The files inside *pearl-config* are also **optional** scripts:

- *install.sh* - contains the [hooks functions](#hook-functions) executed during the *install*, *update* and *remove* events.
- *config.sh* - will be sourced whenever a new Bash/Zsh shell is starting up.
- *config.bash* - will be sourced whenever a new Bash shell is starting up.
- *config.zsh* - will be sourced whenever a new Zsh shell is starting up.
- *config.fish* - will be sourced whenever a new Fish shell is starting up.
- *config.vim* - will be executed whenever Vim editor is starting up.
- *config.el* - will be sourced whenever Emacs editor is starting up.

The following variables can be used in any of the previous scripts:

- *PEARL_HOME*          - Pearl location (default: *$HOME/.config/pearl*)
- *PEARL_ROOT*          - Pearl script location
- *PEARL_PKGDIR*        - Pearl package location
- *PEARL_PKGVARDIR*     - Pearl package location containing data needed for package
- *PEARL_PKGNAME*       - Pearl package name
- *PEARL_PKGREPONAME*   - Pearl package repo name (useful to detect and interact with packages within the same repo)

Additionally, the script *install.sh* can use the utility functions available in
[Buava](https://github.com/fsquillace/buava) and Pearl [*utils*](lib/utils) directory that
make easier the integration with Pearl ecosystem.

Useful examples of Pearl packages can be checked in the
[Official Pearl Hub](https://github.com/pearl-hub).

**Note**: Legacy Pearl versions were using a different directory named *pearl-metadata*. This directory is meant to be deprecated in the upcoming Pearl version.

### The install.sh script ###
#### Hook functions ####
- *post_install*  - Called *after* an installation of the package occurs.
- *pre_update*    - Called *before* an update of the package occurs.
- *post_update*   - Called *after* an update of the package occurs.
- *pre_remove*    - Called *before* a removal of the package occurs.
- *post_remove*   - Called *after* a removal of the package occurs.

#### An install.sh script example ####

    post_install() {
        info "Awesome - new package installed!"
        warn "Remember to setup your config located in: ~/.dotfiles"
        link tmux "$PEARL_PKGDIR/mytmux.conf"
    }
    post_update() {
        post_install
    }
    pre_remove() {
        info "dotfiles package removed"
        unlink tmux "$PEARL_PKGDIR/mytmux.conf"
    }

The `info` and `warn` are functions that print a message
using different colors (namely cyan and yellow).

The `link` `unlink` are idempotent functions (the result will not change
if the function will be called multiple times) that are able
to link/unlink a config file in order to be loaded at startup by a certain program.

All these functions belong to the [Buava](https://github.com/fsquillace/buava) package
in [*utils.sh*](https://github.com/fsquillace/buava/blob/master/lib/utils.sh) and to
the Pearl [*utils.sh*](lib/utils/utils.sh) script.

## Create a Pearl package from a local directory ##
Pearl package system will work even for local directories. This is particularly useful
whenever a Pearl package needs to be tested before pushing to a git repository.

For instance, the following lines in *pearl.conf* file will add a package located in
*/home/joe/dotfiles*:

    PEARL_PACKAGES["joe-dotfiles"]="/home/joe/dotfiles"
    PEARL_PACKAGES_DESCR["joe-dotfiles"]="The Joe's dotfiles"

The directory path must be an absolute path.

The package will be ready to be [installed](#install), [updated](#update)
and [removed](#remove) via the Pearl system.

The directory content can be structured in the exact way as described
in the previous [section](#structure-of-a-pearl-package).

## Use third-party git repository not available in Pearl Hub ##
If you want to use a third-party git repository
that is not available in the [Official Pearl Hub](https://github.com/pearl-hub),
you can:

* Create your own git repository and use the *PEARL_PKGVARDIR* directory (recommended)
* Create your own git repository and use [git submodule](https://git-scm.com/docs/git-submodule)
* Point directly to the third-party git repository

To see examples of Pearl packages from third-party git repos take a look at the
[Official Pearl Hub](https://github.com/pearl-hub).

### Create your own git repository and use the *PEARL_PKGVARDIR* directory (recommended) ###
You can use the *PEARL_PKGVARDIR* directory during the installation phase to install the third-party git repository.
This is the best way to incorporate third-party project into Pearl ecosystem.

Here it is an example of *install.sh* file which install the ranger file manager into the directory *${PEARL_PKGVARDIR}/ranger*:

    function post_install(){
        install_or_update_git_repo https://github.com/ranger/ranger.git "${PEARL_PKGVARDIR}/ranger" master
    }

    function post_update(){
        post_install
    }

    function pre_remove(){
        rm -rf ${PEARL_PKGVARDIR}/ranger
    }

The function `install_or_update_git_repo` comes from the [Buava](https://github.com/fsquillace/buava)
library in [*utils.sh*](https://github.com/fsquillace/buava/blob/master/lib/utils.sh)
which is natively available in Pearl during the installation.
You can even use the functions `install_git_repo` or `update_git_repo` which respectively install or update the git repository.

For a full example take a look at the [ranger](https://github.com/pearl-hub/ranger) Pearl Hub package.

### Create your own git repository and use git submodule ###
Inside your git repository, you just need to add the third-party git repo as a
[git submodule](https://git-scm.com/docs/git-submodule).
For instance, to add the [powerline](https://github.com/powerline/powerline) in your Pearl package,
you can introduce a submodule in the *module* directory:

    git submodule add https://github.com/powerline/powerline.git module

The filesystem structure of the package will become something like this:

    / (package root)
    │
    ├── pearl-config   (optional directory)
    ├── module/        (contains third-party code)
    └── (additional package content)

Then, you just need to modify the config scripts in order to integrate the third-party
project inside Pearl environment.

### Point directly to the third-party git repository ###
Let's suppose you want to install the [vim-rails](https://github.com/tpope/vim-rails) plugin.
In your Pearl configuration (~/.config/pearl/pearl.conf), add your new Pearl package:

    PEARL_PACKAGES["vim-rails"]="https://github.com/tpope/vim-rails.git"
    PEARL_PACKAGES_DESCR["vim-rails"]="Ruby on Rails power tools"

Install the package:

    pearl install vim-rails

Voila', your new vim plugin is ready to be used!

This approach is particularly useful whenever you do not need to specify
any pearl config to *"enrich"* the third-party project inside
the Pearl environment.


Create your own Pearl repository in seconds!
===============
A Pearl repository is just a git repository containing a file located in *pearl-config/pearl.conf*
with a list of packages. For instance, the *OPH* repository is available
[here](https://github.com/pearl-hub/repo).

In order to use the new repository (i.e. "https://github.com/myrepo/pearl-repo.git"),
update the *pearl.conf* file by adding the following line:

    PEARL_REPOS+=("https://github.com/myrepo/pearl-repo.git")

Troubleshooting
===============

## Corrupted Pearl Home directory ##

> **Q**: What should I do if I accidentally removed files/packages in `$PEARL_HOME`?

> **A**: You can recover the structure of the `$PEARL_HOME` by running:

    $> pearl init

> The command will create all the essential directories and symlinks in `$PEARL_HOME`.
> It is harmless to run the `init` command multiple times since it is idempotent.

## Corrupted package ##

> **Q**: Why I can no longer update/remove a package?

> **A**: This is probably because either one of the hook functions
> is failing or the package content is corrupted. You can forcely delete a package by simply
> removing its directory:

    $> rm -rf $PEARL_HOME/packages/pearl/<packagename>

> After that, you can reinstall the package again.
> The Pearl packages contain a dedicated directory `var` for storing
> data needed for the package itself.
> The `var` data are always managed by the package and they never gets deleted by Pearl
> during the package removal.
> If you want to delete the content in `var` package:

    $> rm -rf $PEARL_HOME/var/pearl/<packagename>

## Overridden USR1 signal ##

> **Q**: Why does Pearl override the USR1 signal every time I open a new shell?

> **A**: Pearl needs to trap the `USR1` signal in order to communicate with
> the shell for reloading the Pearl configuration after installing, updating and removing packages.
> Pearl will continue operating normally as it will override any existing trap,
> but you may want to check out if other applications require to trap `USR1` signal
> and resolve the conflict eventually.

Contributing
============

You could help improving Pearl and the [OPH](https://github.com/pearl-hub) in the following ways:

- [Reporting Bugs](CONTRIBUTING.md#reporting-bugs)
- [Suggesting Enhancements](CONTRIBUTING.md#suggesting-enhancements)
- [Writing Code](CONTRIBUTING.md#your-first-code-contribution)
