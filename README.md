Pearl
=====

Because only in the best Shells you will find a Pearl...

```sh
curl -sL https://git.io/vV4yE | sh
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
- [Create your own Pearl package in seconds!](#create-your-own-pearl-package-in-seconds)
- [Create your own Pearl repository in seconds!](#create-your-own-pearl-repository-in-seconds)
- [Troubleshooting](#troubleshooting)

Description
===========
**Pearl** is a package manager for dotfiles, plugins, programs
and any form of source code accessible via git.

As soon as a package is installed, its content can be activated out of the box
according to certain events, like for instance a shell startup (Bash, Zsh or Fish) or
an editor startup (vim or emacs). This is possible via a smart and simple
[hook mechanism](#create-your-own-pearl-package-in-seconds)
that integrates the package content within the Pearl ecosystem.

The main advantages on using Pearl are:

- Create your own Pearl package ***in seconds*** (any git repository is already a Pearl package)!
- Full control and sync of your dotfiles across different systems.
- No more hassles to use different package/plugin managers that only control specific editors, shells, etc.
- Automatic booting of the package content whenever shells or editors are started.
- Access to a wide range of existing packages via the [OPH (Official Pearl Hub)](https://github.com/pearl-hub).
- Allows to create your own package repository that can be shared with your friends!

Quickstart
==========
The Pearl CLI script allows to: *list*, *search*, *install*, *update*,
*remove* the Pearl packages defined according to the configuration located in *~/.config/pearl/pearl.conf*

List
----
- List all the available packages:

```sh
$ pearl list
...
pearl/sesaila [installed]
    Awesome aliases for Bash, Zsh and Fish shells (https://github.com/pearl-hub/sesaila)
pearl/git
    Awesome git dotfiles (https://github.com/pearl-hub/git)
pearl/airline [installed]
    Status/tabline for vim (https://github.com/vim-airline/vim-airline)
pearl/trash [installed]
    Smart command to recover files you regretted to delete (https://github.com/pearl-hub/trash)
...
```

Search
------
- Search for vim Pearl packages:

```sh
$ pearl search vim
* Updating https://github.com/pearl-hub/repo.git repository
pearl/vim
    Awesome vim dotfiles (https://github.com/pearl-hub/vim)
```

Install
-------
- Install pearl/vim package (as soon as the package is installed the package is ready out of the box in vim editor!):

```sh
$ pearl install vim
* Updating https://github.com/pearl-hub/repo.git repository
* Installing pearl/vim package
```

- Install pearl/trash package:

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
- Update pearl/vim package:

```sh
$ pearl update vim
* Updating https://github.com/pearl-hub/repo.git repository
* Updating pearl/vim package
```

- Update Pearl and all its packages installed:

```sh
$ pearl update
...
* Updating https://github.com/pearl-hub/repo.git repository
* Updating pearl/vim package
* Updating pearl/airline package
* Updating pearl/trash package
* Updating pearl/caprica package
...
```

Remove
-------
- Remove pearl/vim package:

```sh
$ pearl remove vim
* Updating https://github.com/pearl-hub/repo.git repository
* Removing pearl/vim package
* Removing pearl/airline package
* Removing pearl/trash package
* Removing pearl/caprica package
```

- Remove Pearl and all its packages installed:

```sh
$ pearl remove
...
Are you sure to REMOVE all the Pearl packages in $PEARL_HOME folder? (N/y)
* Updating https://github.com/pearl-hub/repo.git repository
* Removing pearl/vim package
...
...
```

Installation
============

```sh
curl -sL https://git.io/vV4yE | sh
```

Dependencies
------------
The main Pearl dependencies are the following:

- [bash (>=4.2)](https://www.gnu.org/software/bash/)
- [git (>=1.8)](https://git-scm.com/)
- [coreutils](https://www.gnu.org/software/coreutils/)
- [grep](https://www.gnu.org/software/grep/)

Create your own Pearl package in seconds!
===============
**Any git repository is already a Pearl package**. For instance, in order
to see a dotfiles repository in Pearl, you just need to change
the Pearl configuration file located in *$HOME/.config/pearl/pearl.conf*.

Add the following line to *pearl.conf* file:

    PEARL_PACKAGES["joe-dotfiles"]="https://github.com/joe/mydotfiles.git"

In other words, update the `PEARL_PACKAGES` array with a new entry containing the
name of the package (i.e. *joe-dotfiles*) and the git url (i.e. *https://github.com/joe/mydotfiles.git*).

***That's it!*** The package will be ready to be installed, updated and removed via the Pearl system.

Also, an optional description of the package can be defined via `PEARL_PACKAGES_DESCR` array:

    PEARL_PACKAGES_DESCR["joe-dotfiles"]="The Joe's dotfiles"

## Structure of a Pearl package ##
Your own git repository can contain an **optional** directory
named *pearl-metadata* used by Pearl to integrate the package with the Pearl environment.

    / (package root)
    │
    ├── pearl-metadata (optional directory)
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

The metadata files are also **optional** scripts:

- *install.sh* - contains the [hooks functions](#hook-functions) executed during the *install*, *update* and *remove* events.
- *config.sh* - will be sourced whenever a new Bash/Zsh shell is starting up.
- *config.bash* - will be sourced whenever a new Bash shell is starting up.
- *config.zsh* - will be sourced whenever a new Zsh shell is starting up.
- *config.fish* - will be sourced whenever a new Fish shell is starting up.
- *config.vim* - will be executed whenever vim editor is starting up.
- *config.el* - (coming soon!) will be sourced whenever emacs editor is starting up.

The following variables can be used in any of the previous scripts:

- *PEARL_HOME*    - Pearl location (default: *$HOME/.config/pearl*)
- *PEARL_ROOT*    - Pearl script location
- *PEARL_PKGDIR*  - Pearl package location

Additionally, the scripts can use the utility functions in *utils.sh* that
make easier the integration with Pearl ecosystem.

Useful examples of Pearl packages can be checked in the
[Official Pearl Hub](https://github.com/pearl-hub).

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
        apply "source $PEARL_PKGDIR/mytmux.conf" ${HOME}/.tmux.conf
    }
    post_update() {
        post_install
    }
    pre_remove() {
        info "dotfiles package removed"
        unapply "source $PEARL_PKGDIR/mytmux.conf" ${HOME}/.tmux.conf
    }

The `info` and `warn` are functions that print a message
using different colors (namely white and yellow).

The `apply` `unapply` are idempotent functions (the result will not change
if the function will be called multiple times) that are able
to insert/remove a line in a config file.

All these functions belong to the *utils.sh* script.

## Use third-party project not available in Pearl Hub ##
If you want to use a third-party project
that is not available in the [Official Pearl Hub](https://github.com/pearl-hub),
you can:

* Point directly to the third-party project git repository
* Create your own git repository and use [git submodule](https://git-scm.com/docs/git-submodule)

### Point directly to the third-party project git repository ###
Let's suppose you want to install the [vim-rails](https://github.com/tpope/vim-rails) plugin.
In your Pearl configuration (~/.config/pearl/pearl.conf), add your new Pearl package:

    PEARL_PACKAGES["vim-rails"]="https://github.com/tpope/vim-rails.git"
    PEARL_PACKAGES_DESCR["vim-rails"]="Ruby on Rails power tools"

Install the package:

    pearl install vim-rails

Voila', your new vim plugin is ready to be used!

This approach is particularly useful whenever you do not need to specify
any pearl metadata to *"enrich"* the third-party project inside
the Pearl environment.

### Create your own git repository and use git submodule ###
Inside your git repository, you just need to add the third-party project as a
[git submodule](https://git-scm.com/docs/git-submodule).
For instance, to add the [powerline](https://github.com/powerline/powerline) in your Pearl package,
you can introduce a submodule in the *module* directory:

    git submodule add https://github.com/powerline/powerline.git module

The filesystem structure of the package will become something like this:

    / (package root)
    │
    ├── pearl-metadata (optional directory)
    ├── module/        (contains third-party code)
    └── (additional package content)

Then, you just need to modify the metadata scripts in order to integrate the third-party
project inside Pearl environment.

To see examples of Pearl packages from third-party projects take a look at the
[Official Pearl Hub](https://github.com/pearl-hub).

Create your own Pearl repository in seconds!
===============
A Pearl repository is just a git repository containing a file named *repo.conf*
with a list of packages. For instance, the *OPH* repository is available
[here](https://github.com/pearl-hub/repo).

In order to use the new repository (i.e. "https://github.com/myrepo/pearl-repo.git"),
update the *pearl.conf* file by adding the following line:

    PEARL_REPOS+=("https://github.com/myrepo/pearl-repo.git")

Troubleshooting
===============

This section has been left blank intentionally.
It will be filled up as soon as troubles come in!
