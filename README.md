![Color El-Get logo](https://raw.github.com/dimitri/el-get/master/logo/el-get.png)
El-Get allows you to install and manage `elisp` code for Emacs. It supports
lots of differents types of sources and is able to *install* them, *update*
them and *remove* them, but more importantly it will *init* them for you.
That means it will `require` the *features* you need, `load` the necessary
files, set the *Info* paths so that `C-h i` shows the new documentation you
now depend on, and finally call your own initialisation code for you to
setup the extension. Or call it a package.

# Introduction

There are many methods to keep track of your emacs setup.  You can manage it
all in a private git repository, you can set up `git submodules` or directly
import external repositories.  You can manually retrieve the various
packages you wish to track and ensure they are installed on any machine you
apply your configuration to.

All of these systems require some degree of manual maintenance, especially
if you have packages from various types of locations:
[github](http://github.com), [emacswiki](http://emacswiki.org),
[GNU ELPA](http://elpa.gnu.org/) or [Marmalade](http://marmalade-repo.org/),
privately hosted pages, [git](http://git-scm.com/),
[bzr](http://bazaar.canonical.com/en/), [CVS](http://www.nongnu.org/cvs/),
the list goes on.

El-Get is designed to simplify this process and allow access to all the
various methods of obtaining packages from a single interface.  Every
package has a recipe that allows you to locate the original source, and that
can be updated if the package is moved.

Whether you are using one machine or many, El-Get provides you with a simple
interface to your list of installed packages, and the tools to keep them up
to date.

# El-Get, ELPA and package.el

Emacs 24 ships with `package.el` which allows for easy installation of Emacs
Lisp extensions for Emacs, and supports several servers where to find a list
of packaged extension.

Rather than ask authors or contributors to clean-up and package existing
software, the El-Get approach is to take bits and pieces as they exist today
and still empower Emacs users in a way that those random electrons are easy
to use.

That's why El-Get supports `package.el` as one of its *methods* to fetch
Emacs Lisp Extensions.

Technical differences also include the ability for El-Get to run OS commands
(such as `make` or `ginstall-info`) so as to better cope with the diversity
found in the wild, allowing for automatic inclusion of *Info* pages for
packages providing some.

# Installation

El-Get is easy to install.  The only requirements to do so successfully are
Emacs, `git` and a connection to the internet that allows you to `git clone`
repositories.

If you do not already have `git` on your system, you can install it through
your package manager if you are using Linux or by downloading it from the
[Git Homepage](http://git-scm.com/).

## Installation Dependencies

Installing El-Get depends on a working `install-info` command, please make
sure you have one in your `PATH`. In `debian`, it's available in the
[install-info debian package](http://packages.debian.org/squeeze/install-info). The
[MacOSX install-info](http://developer.apple.com/library/mac/#DOCUMENTATION/Darwin/Reference/ManPages/man1/install-info.1.html)
version works fine with El-Get.

When using the *windows* operating system, take into account that the way
Emacs calls external programs is not the same for *native builds* and
*cygwin*, so make sure you don't mix and match them at least for
`install-info` (e.g. *cygwin* version of `install-info` will error out when
called by el-get from a `windows-nt` Emacs, see `system-type`). When using a
*native build* of Emacs for windows, consider using the
[GNU Win 32](http://gnuwin32.sourceforge.net/packages.html) distribution of
[TexInfo for windows](http://gnuwin32.sourceforge.net/packages/texinfo.htm),
which contains the proper `install-info` version when you're not using the
*cygwin* Emacs binary.

## Stable Branch

To install El-Get you can use the *lazy-installer*.  This will not load it
on startup or otherwise affect future usage of Emacs.  If you wish to ensure
that El-Get will be available in future Emacs session please use the code
provided in **Basic Setup**.  Using the code below will require an internet
connection even if El-Get is already installed, that's why it's adviced to
use it for first time installation, not for embedding into your `.emacs` (or
your `user-init-file`).

```lisp
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.
(url-retrieve
 "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp)))
```

Evaluating this code after copying it into your `*scratch*` buffer by typing
`C-j` or `M-x eval-print-last-exp` will retrieve the El-Get installation
script.  This script will then use `git` to clone El-Get and install it to
the default location (`~/.emacs.d/el-get/el-get`).

## Master Branch

The lazy installer above targets the current stable release.  If you would
rather use the current development version you must clone the `master`
branch by ensuring the variable `el-get-master-branch` exists.

```lisp
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working developper edition of el-get.
(url-retrieve
 "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (let (el-get-master-branch)
     (goto-char (point-max))
     (eval-print-last-sexp))))
```

## Upgrading from 3.1 to 4.1

The development of El-Get 4.1 took a long time, and as a result a lot of
recipes have change in non compatible ways: some sources switched from `SVN`
to `git`, some revisited their hosting choice, etc.

As a result, lots of recipe should be *reinstalled* when upgrading. The
easiest way here might well be to just backup your `el-get-dir` directory
and start-up fresh with the new El-Get code:

    mv ~/.emacs.d/el-get ~/.emacs.d/el-get-backup-3.stable
    mkdir ~/.emacs.d/el-get
    M-x el-get-self-update

That code sample assumes that `el-get-dir` is set to its default value, that
is `~/.emacs.d/el-get`.

# Setup

## Basic Setup

If you wish to ensure that El-Get is available when you load Emacs you can
place the following elisp code in your init file.  It will detect if
`el-get` is already installed and install it if necessary.

The addition of `(el-get 'sync)` in the code blocks below ensures that any
currently *installed* packages will be initialized and any *required*
packages will be installed.

Calling the `el-get` function is covered in details in the full *Info*
manual.

Here is the basic setup to add to your `user-init-file` (`.emacs`):

```lisp
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
```

And for those who prefer the master branch, please use the code below

```lisp
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)
```

## Package Setup

The easiest way to setup a given package is to add its initialization code
to a file named `init-<package>.el` with `<package>` replaced with the
package name. This file needs to be placed in the directory
`el-get-user-package-directory` (defaults to `nil`, you have to set a value
for it, like for example `~/.emacs.d/el-get-init-files/`).

El-Get will then load that file at package initialization time. See the full
*Info* documentation for more details and possibilities.

# Usage

El-Get requires very little interaction with your init file when managing
packages.  **Basic Usage** explains how to manage your packages without ever
having to touch your init file again (meaning, *once El-Get is
installed*). Please refer to the *Info* documentation provided with El-Get
if you think you need to edit your init file (when sharing the same setup
between several machines for example).

## Basic usage

### Adding and removing packages

- **M-x el-get-install**

   Will prompt for a package name, with completion, then install it.  It
   will only propose packages that are not already `installed`.  Any package
   that you have a recipe for is a candidate.

- **M-x el-get-remove**

   Will prompt for an `installed` package name, with completion, then remove
   it. Depending on the `type` of the package, this often means simply
   deleting the directory where the source package lies. Sometime we have to
   use external tools instead (e.g. `apt-get`). No effort is made to unload
   the features.

- **M-x el-get-reinstall**

   This is just a shortcut for `el-get-remove` followed by `el-get-install`
   of the same package. It is primarily useful when a package has changed
   types, so the normal `el-get-update` process will not work correctly.

### Keeping up to date

- **M-x el-get-self-update**

   Update only one package, `el-get` itself.

- **M-x el-get-update**

   Will prompt for an installed package name, with completion, then update
   it. This will run the `build` commands and `init` the package again.

- **M-x el-get-update-all**

   Will update all packages that have the `installed` status in your status
   file.  Before the update you will be prompted for confirmation that you
   wish to proceed.

   Beware that using this function can lead to hours of settings review:
   more often than not updating a package requires some adjustments to your
   setup.  Updating all of them at once will require reviewing almost all
   your setup.

- **M-x el-get-reload**

   Reload the given package files.  Happens automatically at update time
   too.

### Viewing available recipes

- **M-x el-get-list-packages**

   Opens a buffer listing all known packages (those for which you have a
   recipe).  The listing includes the package name, its status (one of
   *available*, *installed*, *removed* or *required*) and the package
   description.  The description is a free form text and has not been
   provided for all recipes.

- **M-x el-get-describe**

   Prompt for a package name, with completion, then open an `*Help*` window
   with details about the selected package.  Those include current status,
   website, description, installation method, full recipe, and buttons to
   easily install, update or remove the package.

- **M-x el-get-find-recipe-file**

   Will prompt for the name of a package, with completion, then `find-file`
   its `recipe` file.  If the recipe does not exist, it will create a new
   recipe file with the appropriate name.

# Conclusion

Enjoy El-get, enjoy Emacs, have fun with Emacs Lisp, and simplify your Emacs
Setup **today**!
