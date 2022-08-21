Pearl
=====

<h1 align="center">
    <a href="https://github.com/pearl-core/pearl"><img
        alt="Pearl"
        width=250px
        src="https://rawgit.com/pearl-core/logo/master/pearl.png"></a>
</h1>

|Project Status|Donation|Communication|
|:-----------:|:--------:|:-----------:|
|[![Build status](https://api.travis-ci.com/pearl-core/pearl.png?branch=master)](https://travis-ci.com/github/pearl-core/pearl) [![PyPi version](https://img.shields.io/pypi/v/pearl)](https://pypi.org/project/pearl/) [![PyPi status](https://img.shields.io/pypi/status/pearl)](https://pypi.org/project/pearl/) | [![Github Sponsors](https://img.shields.io/badge/GitHub-Sponsors-orange.svg)](https://github.com/sponsors/fsquillace) [![PayPal](https://img.shields.io/badge/PayPal-Donation-blue.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=8LEHQKBCYTACY) | [![Join the gitter chat at https://gitter.im/pearl-core/pearl](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/pearl-core/pearl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) |

**Table of Contents**
- [Description](#description)
- [Quickstart](#quickstart)
- [Installation](#installation)
  - [Dependencies](#dependencies)
  - [Linux](#linux)
  - [OSX](#osx)
- [Create your own Pearl package](#create-your-own-pearl-package)
- [Create your own Pearl repository](#create-your-own-pearl-repository)
- [Comparison with alternative solutions](#comparison-with-alternative-solutions)
- [Troubleshooting](#troubleshooting)
- [Contributing](#contributing)
- [Donating](#donating)
- [Authors](#authors)

Description
===========
**Pearl** is a lightweight package manager for automating reproducible environments
between different systems (Linux and OSX).
It can be used for dotfiles, plugins, programs and any form of code
accessible via git.

As soon as a package gets installed, its content can be activated out of the box
according to certain events, like, for instance, a shell startup (Bash, Zsh or Fish) or
an editor startup (Vim or Emacs). This is possible via a smart and simple
[hook mechanism](#create-your-own-pearl-package)
that integrates the package content within the Pearl ecosystem.

The main advantages on using Pearl are:

- Create your own Pearl package in a very simple way.
- Full control and sync of your dotfiles across different systems.
- Automatic bootstrap of the package content whenever shells or editors get started.
- Access to a wide range of existing packages via the [OPH (Official Pearl Hub)](https://github.com/pearl-hub).
- Allows to create your own shareable package repository.
- [Comparison](#comparison-with-alternative-solutions) with alternative solutions
- Stable codebase with 100+ unit tests and exhaustive integration tests via [Travis](https://travis-ci.org/pearl-core/pearl) for Linux and OSX.
- Small number of [dependencies](#dependencies) needed in order to ensure compatibility with most of the systems.

Quickstart
==========
There are two main use cases for Pearl which will be explained here below:

Use case 1: Create custom package
---------------------
The following example creates a Pearl package containing
a `git` dotfile and a simple executable available as soon
as the package gets installed.

```sh
$> pearl create mydotfiles ~/dotfiles
```
This will create a directory `pearl-config` in `~/dotfiles`
containing all the templates to help you
start writing a Pearl package. `~/dotfiles` does not need to be an empty directory.

Additionally, the local repository in `$XDG_CONFIG_HOME/pearl/pearl.conf` (defaults to `~/.config/pearl/pearl.conf`)
will be updated with the new package entry called `mydotfiles`.
This tells to Pearl where to look for the package:

```sh
$> cat ~/.config/pearl/pearl.conf
...
...
PEARL_PACKAGES["mydotfiles"] = {"url": "~/dotfiles"}
```

Place the git config inside `~/dotfiles` directory:

```sh
$> cd ~/dotfiles
$> echo -e "[alias]\n    cfg = config" > gitconfig
$> echo -e "#!/bin/bash\necho Hello World!" > hello
$> chmod +x hello
```

You need now to give instructions about how to link the `gitconfig` file into the system and make the executable available in `PATH`.
This is possible through the `pearl-config/hooks.sh` file. Just update it with the following:

```bash
post_install() {
    link git "${PEARL_PKGDIR}/gitconfig"
    link_to_path "${PEARL_PKGDIR}/hello"
    return 0
}

post_update() {
    post_install
}

pre_remove() {
    unlink git "${PEARL_PKGDIR}/gitconfig"
    unlink_from_path "${PEARL_PKGDIR}/hello"
    return 0
}
```

This tells to Pearl to `link` the git config located in `"${PEARL_PKGDIR}/gitconfig"` (`${PEARL_PKGDIR}` is a builtin variable)
to the `git` program just after the package installation.
Also, `hooks.sh` will link the executable `hello` by creating
a symlink and make it visible to `PATH` env variable.
Conversely, before removal, the hooks file tells to
`unlink` the git config file and remove the symlink.

Now, just install the package and you will see the changes already reflected:

```sh
$> pearl install mydotfiles
$> git cfg -l
...
...
$> hello
Hello World!
```

Once the package is completed, you can upload it to a git repository and
just fetch it from there by updating `~/.config/pearl/pearl.conf`:
```sh
$> cat ~/.config/pearl/pearl.conf
...
...
PEARL_PACKAGES["mydotfiles"] = {"url": "https://github.com/pearluser/mydotfiles.git"}
```

There are way more things you can do with Pearl!
For more details about the `pearl-config` content, look at the [section](#create-your-own-pearl-package) below.

Use case 2: Use Pearl Hub repository
------------------------
You can just use existing packages from the Pearl Hub repository.
It contains a big list of packages about dotfiles, programs and plugins for many known applications.

For instance, look to the entire list of packages:

```sh
$> pearl list
```

If interested to search only for dotfiles:

```sh
$> pearl search dotfiles
pearl/dot-gtk 
    Awesome gtk dotfiles
pearl/kyrat 
    20 lines script that brings dotfiles in a ssh session
pearl/dot-mutt 
    Awesome Mutt dotfiles
pearl/dot-emacs 
    Awesome emacs dotfiles
pearl/dot-git 
    Awesome git dotfiles
pearl/dot-screen 
    Awesome screen dotfiles
pearl/dot-tmux
    Awesome Tmux dotfiles
pearl/dot-vim
    Awesome vim dotfiles
pearl/dot-firefox
    Awesome Firefox dotfiles
pearl/dot-terms
    Awesome terms dotfiles (i.e. urxvt)
pearl/dot-bash
    Awesome bash dotfiles
```

### Recommended Pearl Hub packages to install:

- [cmd](https://github.com/pearl-hub/cmd)
- [kyrat](https://github.com/pearl-hub/kyrat)
- [ranger](https://github.com/pearl-hub/ranger)
- [sesaila](https://github.com/pearl-hub/sesaila)
- [trash-cli](https://github.com/pearl-hub/trash-cli)
- [txum](https://github.com/pearl-hub/txum)

Installation
============

Dependencies
------------
Before installing Pearl be sure that all dependencies are properly installed in your system.
The Pearl dependencies are the following:

### Mandatory
- [python (>=3.7)](https://www.python.org/)
- [bash (>=4.1)](https://www.gnu.org/software/bash/)
- [git (>=1.8.5)](https://git-scm.com/)

**PLEASE NOTE**: Tests may be performed on different versions from the ones listed above.
To know which versions are truly tested have a look at latest Travis executions [here](https://travis-ci.com/pearl-core/pearl).

### Optional
The following are not mandatory dependencies but can be handy to have for the hook functions in Pearl package.
All the Linux distributions have these dependencies already installed.

- [GNU coreutils](https://www.gnu.org/software/coreutils/)
- [grep](https://www.gnu.org/software/grep/) 
- [sed](https://www.gnu.org/software/sed/) 

### Shells supported
Pearl supports the following shells:

- [bash (>=4.1)](https://www.gnu.org/software/bash/)
- [fish (>=2.2.0)](https://fishshell.com/)
- [zsh (>=5.2)](http://www.zsh.org/)

**PLEASE NOTE**: Tests may be performed on different versions from the ones listed above.
To know which versions are truly tested have a look at latest Travis executions [here](https://travis-ci.com/pearl-core/pearl).

Linux
-----

### Arch Linux

Pearl can be installed in Arch Linux through AUR.
The package is [pearl-git](https://aur.archlinux.org/packages/pearl-git/).

For example, to install Pearl via [yay](https://github.com/Jguer/yay) AUR helper:
```
$> yay -S pearl-git
```

Any other AUR helpers can be found [here](https://wiki.archlinux.org/index.php/AUR_helpers).

### Other Linux distributions

Assuming all Pearl [dependencies](#dependencies) are properly installed
in the system, to install Pearl you can use the `pip` command:

```
$> sudo python3 -m pip install pearl
$> pearl init
```

Make sure to update `PATH` environment variable if needed
(`pearl` is typically located to `/usr/bin`).
The idempotent `init` command will create the `$PEARL_HOME` directory and
the new pearl configuration files from template.

OSX
---
In order to install all Pearl dependencies, you first need to install [Homebrew](http://brew.sh/).

To install all the needed dependencies via Homebrew:
```sh
$> brew update
$> brew install bash git coreutils grep gnu-sed python
```

To install Pearl you can use the `pip` command:
```sh
$> pip3 install pearl
$> pearl init
```

Make sure to update `PATH` environment variable if needed
(`pearl` is typically located to `/usr/bin`).
The idempotent `init` command will create the `$PEARL_HOME` directory and
the new pearl configuration files from template.

**IMPORTANT NOTE**: Pearl gets loaded through `~/.bashrc`. The problem is that in OSX,
the terminal opens a login shell and only `~/.bash_profile` will get executed.
Run the following only if `~/.bashrc` is not loaded within `~/.bash_profile` file:

```sh
$> echo "[[ -f $HOME/.bashrc ]] && source $HOME/.bashrc" >> ~/.bash_profile
```

This will make sure that `~/.bashrc` will run at shell startup.

Create your own Pearl package
===============
**Any git repository is already a Pearl package**. For instance, in order
to manage a dotfiles repository in Pearl, you just need to change
the Pearl configuration file located in `$XDG_CONFIG_HOME/pearl/pearl.conf`.

Add the following line to `pearl.conf` file:

```python
PEARL_PACKAGES = {
    "mydotfiles": {
        "url": "https://github.com/user/mydotfiles.git",
        "description": "My dotfiles"
    },
}
```

In other words, update the `PEARL_PACKAGES` dictionary with a new entry containing the
name of the package (i.e. `mydotfiles`),
the git url (i.e. `https://github.com/user/mydotfiles.git`) and an optional description.

***That's it!*** The package will be ready to be managed by the Pearl system.

## Structure of a Pearl package ##
Your own git repository can contain an **optional** directory
named `pearl-config` used by Pearl to integrate the package with the Pearl environment.

    / (package root)
    │
    ├── pearl-config (optional directory)
    │   │
    │   ├── hooks.sh
    │   ├── config.sh
    │   ├── config.bash
    │   ├── config.zsh
    │   ├── config.fish
    │   ├── config.vim
    │   ├── config.el
    │   └── package.conf
    │
    └── (additional package content)

The files inside `pearl-config` are also **optional** script/configuration files:

- `hooks.sh` - contains the [hooks functions](#hook-functions) executed during the `install`, `update` and `remove` events.
- `config.sh` - will be sourced whenever a new Bash/Zsh shell is starting up.
- `config.bash` - will be sourced whenever a new Bash shell is starting up.
- `config.zsh` - will be sourced whenever a new Zsh shell is starting up.
- `config.fish` - will be sourced whenever a new Fish shell is starting up.
- `config.vim` - will be executed whenever Vim editor is starting up.
- `config.el` - will be sourced whenever Emacs editor is starting up.
- `package.conf` - contains optional metadata information (name, author, description, keywords, etc) about the package that are useful when indexing the package in a repository list.

The order in which the `config.*` files are sourced among multiple installed packages depends on the closure dependency tree,
in other words, if package `A` depends on package `B` and both have `config.vim` files, the parent package `B` config file will be sourced first.

The following variables can be used in any of the previous scripts:

- `PEARL_HOME`          - Pearl location (`$XDG_DATA_HOME/pearl` which by default is `$HOME/.local/share/pearl`)
- `PEARL_PKGDIR`        - Pearl package location
- `PEARL_PKGVARDIR`     - Pearl package location containing data needed for package
- `PEARL_PKGNAME`       - Pearl package name
- `PEARL_PKGREPONAME`   - Pearl package repo name (useful to detect and interact with packages within the same repo)

Additionally, the script `hooks.sh` can use the utility functions available in
[Buava](https://github.com/fsquillace/buava) and Pearl [*utils*](lib/utils) directory that
make easier the integration with Pearl ecosystem.

Useful examples of Pearl packages can be checked in the
[Official Pearl Hub](https://github.com/pearl-hub).

### The hooks.sh script ###
#### Hook functions ####
- `post_install`  - Called *after* an installation of the package occurs.
- `pre_update`    - Called *before* an update of the package occurs.
- `post_update`   - Called *after* an update of the package occurs.
- `pre_remove`    - Called *before* a removal of the package occurs.

#### An hooks.sh script example ####

```bash
post_install() {
    warn "Remember to setup your config located in: ~/.dotfile"
    # Do a smart backup before modifying the file
    backup ${HOME}/.dotfile
    "# New dotfile" > ${HOME}/.dotfile
    if ask "Are you sure to link the tmux config?" "Y"
    then
        link tmux "$PEARL_PKGDIR/mytmux.conf"
    fi

    info "Awesome - new package installed!"
    return 0
}
post_update() {
    post_install
    return 0
}
pre_remove() {
    info "dotfiles package removed"
    unlink tmux "$PEARL_PKGDIR/mytmux.conf"

    # Do an idempotent delete
    delete ${HOME}/.dotfile
    return 0
}
```

The `info` and `warn` are functions that print a message
using different colors (namely cyan and yellow).

The `link` `unlink` are idempotent functions (the result will not change
if the function will be called multiple times) that are able
to link/unlink a config file in order to be loaded at startup by a certain program.

The `ask` function will make installation interactive, asking user whether to link tmux config or not.

The `backup` keeps the last three backups of the file and do not perform backup
if the file has not been modified since the latest backup. The `delete` is a
function for idempotent remove (it will not raise an error if the file
no longer exist).

All these functions belong to the [Buava](https://github.com/fsquillace/buava) package
in [`utils.sh`](https://github.com/fsquillace/buava/blob/master/lib/utils.sh)
and to the Pearl [`utils.sh`](lib/utils/utils.sh) script. You can use them
inside the `hooks.sh` to any hook function.

**Very important note**: All the hook functions **must** be
[**idempotent**](https://en.wikipedia.org/wiki/Idempotence)
(the commands of each hook function must produce the same result even if
the command gets executed multiple times).
All buava commands are idempotent and this will help to write hook functions
very quickly.

**Note**: For OSX system, the GNU version `sed` and `grep` are automatically
imported in `hooks.sh` and can be directly used if needed.

### The package.conf file
`package.conf` is located in `pearl-config` directory and is meant to contain package metadata.
To simplify the creation of new packages, this file is completely optional.
This file may contain name of the package, description, author, os compatibility, license and more.
It can be also used to establish dependencies between packages.

Please **note** that `package.conf` is only meant to encapsulate package information within the package itself
but they are not directly consumed by the Pearl program. In fact, Pearl only reads from the `pearl.conf` file which is
where the metadata information can be also stored.

To give a better idea, take a look at the Pearl Hub
[repo.conf](https://github.com/pearl-hub/repo-v2/blob/master/pearl-config/repo.conf).
The [repo-builder](https://github.com/pearl-core/repo-builder/) is the script responsible to periodically
extract the `package.conf` metadata from each package and update the Pearl Hub `repo.conf`.

For a local package you can manually include such information directly in the `pearl.conf` file. This is an example of
package defined in `pearl.conf` which depends on the `pearl/cmd` package:

```python
PEARL_PACKAGES = {
    'mydotfiles': {
        "url": '/home/user/my/folder/dotfiles/',
        "description": "This package contains dotfiles",
        "author": "David Smith <dsmith@abc.com>",
        "depends": (
            "pearl/cmd",
        )
    },
}
```

If you do not need such features for your package, just ignore this file.

## Create a Pearl package from a local directory ##
Pearl package system will work even for local directories. This is particularly useful
whenever a Pearl package needs to be tested before pushing to a git repository.

For instance, the following lines in `pearl.conf` file will add a package located in
`/home/joe/dotfiles`:

```python
PEARL_PACKAGES = {
    "mydotfiles": {
        "url": "/home/user/mydotfiles",
        "description": "My dotfiles"
    },
}
```

The directory path must be an absolute path.

The package will be ready to be managed by the Pearl system.

The directory content can be structured in the exact way as described
in the [section](#structure-of-a-pearl-package) above.

## Use third-party git repository not available in Pearl Hub ##
If you want to use a third-party git repository
that is not available in the [Official Pearl Hub](https://github.com/pearl-hub),
you can:

* Create your own git repository and use the `PEARL_PKGVARDIR` directory (recommended)
* Create your own git repository and use [git submodule](https://git-scm.com/docs/git-submodule)
* Point directly to the third-party git repository

To see examples of Pearl packages from third-party git repos take a look at the
[Official Pearl Hub](https://github.com/pearl-hub).

### Create your own git repository and use the `PEARL_PKGVARDIR` directory (recommended) ###
You can use the `PEARL_PKGVARDIR` directory during the installation phase to install the third-party git repository.
This is the best way to incorporate third-party project into Pearl ecosystem.

Here it is an example of `hooks.sh` file which install the ranger file manager into the directory `${PEARL_PKGVARDIR}/ranger`:

```bash
function post_install(){
    install_or_update_git_repo https://github.com/ranger/ranger.git "${PEARL_PKGVARDIR}/ranger" master
}

function post_update(){
    post_install
}

function pre_remove(){
    rm -rf ${PEARL_PKGVARDIR}/ranger
}
```

The function `install_or_update_git_repo` comes from the [Buava](https://github.com/fsquillace/buava)
library in [`utils.sh`](https://github.com/fsquillace/buava/blob/master/lib/utils.sh)
which is natively available in Pearl during the installation.
You can even use the functions `install_git_repo` or `update_git_repo` which respectively install or update the git repository.

For a full example take a look at the [ranger](https://github.com/pearl-hub/ranger) Pearl Hub package.

### Create your own git repository and use git submodule ###
Inside your git repository, you just need to add the third-party git repo as a
[git submodule](https://git-scm.com/docs/git-submodule).
For instance, to add the [powerline](https://github.com/powerline/powerline) in your Pearl package,
you can introduce a submodule in the `module` directory:

```sh
$> git submodule add https://github.com/powerline/powerline.git module
```

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
In your Pearl configuration (`$XDG_CONFIG_HOME/pearl/pearl.conf`), add your new Pearl package:

```python
PEARL_PACKAGES = {
    "vim-rails": {
        "url": "https://github.com/tpope/vim-rails.git",
        "description": "Ruby on Rails power tools"
    },
}
```

Install the package:

```sh
$> pearl install vim-rails
```

Voila', your new vim plugin is ready to be used!

This approach is particularly useful whenever you do not need to specify
any pearl config to *"enrich"* the third-party project inside
the Pearl environment.


Create your own Pearl repository
===============
A Pearl repository is just a git repository containing a file located in `pearl-config/pearl.conf`
with a list of packages. For instance, the *OPH* repository is available
[here](https://github.com/pearl-hub/repo-v2).

In order to use the new repository (i.e. "https://github.com/myrepo/pearl-repo.git"),
update the `pearl.conf` file by adding the following line:

```python
PEARL_REPOS += ("https://github.com/myrepo/pearl-repo.git",)
```

Comparison with alternative solutions
=====================================
Here we are going to compare Pearl with other solutions according to the following dimensions:

**Modular**

Ability of the tool to split configurations and/or programs into different modules.
Its importance is because in case configs are broken you can still manage other modules which you are more confident they works.
For instance, if your `vim` config does not work you are still able to manage all other dotfiles because they are independent from each other.
Obviously, Pearl, by design, allows modularization through packages.
              
**General purpose**

Tools can be either `general` (manage any kind of programs) or `dotfiles-specific` (limited to dotfiles only).

**Simple**

Indicates how easy is to setup and use the tool. Between all tools, [Ansible](https://www.ansible.com/)
is the one which has a steep learning curve. Ansible is a powerful software for IT
automation which can be widely used for many use cases.
Despite of this, Ansible has few drawbacks when using it for lightweight forms of automation compared to Pearl:

- Pearl uses bash for writing simple scripts for automation:
  - it makes easier the integration with other programs in the system (without existing Playbooks may be hard and tedious to achieve this in Ansible);
  - bash is a powerful, accessible and well-known language;
- Ansible requires way more dependencies than Pearl;
- Ansible requires knowledge about how Ansible Playbooks works;
- Pearl uses built-in [functions](https://github.com/fsquillace/buava/blob/master/README.md#table-of-buava-functions) and [variables](#structure-of-a-pearl-package) which heavily simplify construction of scripts for automation;
- Pearl makes easier to remove packages and restore the system to an initial state;

**Diversity**

Indicates whether the tool handles diverse management for configurations/programs when dealing with heterogeneous machines.
There are multiple ways to handle diversity through Pearl:

- one way is to create just one package and write bash functions which handle specific logic for each machine.
- Alternatively, you can create a base package containing common functionality and use a package specific for each machine.
This is possible thanks to the ability to define dependencies between packages.

**Portable**

Ability of the tools to be used in multiple platforms. Pearl can be used on both Linux and OSX.

Comparison
----------

|                 | Pearl | Ansible | yadm | vcsh | homesick |
| --------------- | ----- | ------- | ---- | ---- | -------- |
| Modular         | Yes   | Yes     | No   | Yes  | Yes      |
| General purpose | Yes   | Yes     | No   | No   | No       |
| Diversity       | Yes   | Yes     | Yes  | Yes  | Yes      |
| Simple          | Yes   | No      | Yes  | Yes  | Yes      |
| Portable        | Yes   | Yes     | Yes  | Yes  | Yes      |

Troubleshooting
===============

## Corrupted Pearl Home directory ##

> **Q**: What should I do if I accidentally removed files/packages in `$PEARL_HOME`?

> **A**: You can recover the structure of the `$PEARL_HOME` by running:

```sh
$> pearl init
```

> The command will create all the essential directories and symlinks in `$PEARL_HOME`.
> It is harmless to run the `init` command multiple times since it is idempotent.

## Corrupted package ##

> **Q**: Why I can no longer update/remove a package?

> **A**: This is probably because either one of the hook functions
> is failing or the package content is corrupted. You can forcely remove the package:

```sh
$> pearl --force remove <packagename>
```

> which bypass hook functions that are failing. If that does not even work,
> you can delete a package by simply removing its directory:

```sh
$> rm -rf $PEARL_HOME/packages/pearl/<packagename>
```

> After that, you can reinstall the package again.
> The Pearl packages contain a dedicated directory `var` for storing
> data needed for the package itself.
> The `var` data are always managed by the package and they never gets deleted by Pearl
> during the package removal.
> If you want to delete the content in `var` package:

```sh
$> rm -rf $PEARL_HOME/var/pearl/<packagename>
```

## Package shell variables/functions not visible in current shell after installation ##

> **Q**: Why are not package's environment variables/functions visible in
> my current shell after installing/updating the package?

> **A**: After package install/update, the variables or
> functions related to the current shell and defined in `pearl-config/config.*`
> may not be available because a reload of Pearl configuration file is required.
> You can fix this by simply run the function:

```sh
pearl-source
```
    
> which reloads the configuration.
> The use of such function is not always required but depends on
> whether the variables/functions involve the current shell where the
> package `install`/`update` occurred (i.e. a new variable defined in `config.sh`
> and the current shell is a bash or zsh). Alternatively, user can always
> create a new shell and the package resources will be available as
> expected.

## Error during package install

> Q: Why Do I get the following error:

```sh
Error on executing 'post_install' hook. Rolling back...
```

> A: This occurs when the `post_install` hook function fails.
> Pearl will attempt to roll back and force a removal of the package. In this way
> you can attempt to install the package again once the hook function gets
> fixed.

## Debugging config files

> Q: How do I debug `config.*` files when running the corresponding program (i.e. `emacs`, `vim`, `bash`, `zsh`, `fish`)?

> A: Set the environment variable `PEARL_DEBUG` before running the program. For example, to check the `config.vim` files run
> when starting up `vim` program:

```sh
PEARL_DEBUG=1 vim
```

## Pearl executable not found

> Q: Why do I get this message when opening shells (bash, zsh, fish) or editors (vim, emacs):

```
Pearl error: Could not load pearl package config files. `pearl` executable not found. Please update the PATH variable first."
```

> A: This is because `pearl` executable is required for properly sourcing the pearl package `config.*` files.
> Make sure to add the path location to the `PATH` in your
> favorite shell config file (i.e. `~/.bashrc`, `~/.zshrc`, `~/.config/fish/config`).

Contributing
============

You could help improving Pearl and the [OPH](https://github.com/pearl-hub) in the following ways:

- [Reporting Bugs](CONTRIBUTING.md#reporting-bugs)
- [Suggesting Enhancements](CONTRIBUTING.md#suggesting-enhancements)
- [Writing Code](CONTRIBUTING.md#your-first-code-contribution)

Donating
========
To sustain the project please consider funding by donations through
the [GitHub Sponsors page](https://github.com/sponsors/fsquillace/).

Authors
=======
Pearl was originally created by [Filippo Squillace (feel.sqoox@gmail.com)](https://github.com/fsquillace).

Here is a list of [**really appreciated contributors**](https://github.com/pearl-core/pearl/graphs/contributors)!

[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/0)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/0)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/1)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/1)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/2)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/2)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/3)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/3)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/4)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/4)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/5)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/5)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/6)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/6)[![](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/images/7)](https://sourcerer.io/fame/fsquillace/pearl-core/pearl/links/7)
