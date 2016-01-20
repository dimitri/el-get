[![Build Status](https://travis-ci.org/dimitri/el-get.svg?branch=master)](https://travis-ci.org/dimitri/el-get)

![Color El-Get logo](https://raw.github.com/dimitri/el-get/master/logo/el-get.png)
El-Get allows you to install and manage `elisp` code for Emacs. It supports
lots of different types of sources and is able to *install* them, *update*
them and *remove* them, but more importantly it will *init* them for you.
That means it will `require` the *features* you need, `load` the necessary
files, set the *Info* paths so that `C-h i` shows the new documentation you
now depend on, and finally call your own initialisation code for you to
setup the extension. Or call it a package.

# Introduction

[![Join the chat at https://gitter.im/dimitri/el-get](https://badges.gitter.im/dimitri/el-get.svg)](https://gitter.im/dimitri/el-get?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

There are many methods to keep track of your emacs setup.  You can manage it
all in a private git repository, you can set up `git submodules` or directly
import external repositories.  You can manually retrieve the various
packages you wish to track and ensure they are installed on any machine you
apply your configuration to.

All of these systems require some degree of manual maintenance, especially
if you have packages from various types of locations:
[github](https://github.com), [emacswiki](http://emacswiki.org),
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
Emacs (23.4 and above), `git` and a connection to the internet that allows you to `git clone`
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

## The Lazy Installer

To install El-Get you can use the *lazy-installer*.  This will not load it
on startup or otherwise affect future usage of Emacs.  If you wish to ensure
that El-Get will be available in future Emacs session please use the code
provided in **Basic Setup**.  Using the code below will require an internet
connection even if El-Get is already installed, that's why it's advised to
use it for first time installation, not for embedding into your `.emacs` (or
your `user-init-file`).

```lisp
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.
(url-retrieve
 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp)))
```

NOTE: if you are using Windows see
[Installation on Windows](https://github.com/dimitri/el-get/wiki/Installation-on-Windows).

Evaluating this code after copying it into your `*scratch*` buffer by typing
`C-j` or `M-x eval-print-last-exp` will retrieve the El-Get installation
script.  This script will then use `git` to clone El-Get and install it to
the default location (`~/.emacs.d/el-get/el-get`).

## Replicating a package set on another Emacs installation

In the Emacs whose setup you wish to replicate, type `M-x ielm` for an
Emacs Lisp prompt, and enter:

```lisp
`(setq my-packages
              ',(mapcar #'el-get-as-symbol
                        (el-get-list-package-names-with-status "installed")))
```

Copy the result into the new Emacs, in which you should already have
installed El-Get, and evaluate it, followed by `(el-get 'sync my-packages)`

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
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
```

### Alternative Basic Setup with Installation via MELPA

```elisp
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
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

Many `init-` packages are already available in El-Get.

# Usage

El-Get requires very little interaction with your init file when managing
packages.  **Basic Usage** explains how to manage your packages without ever
having to touch your init file again (meaning, *once El-Get is
installed*).  **Advanced Usage with Local Recipes** explains how to write
your init file with explicitly specifying packages to install (when sharing
the same setup between several machines for example).

## Basic Usage

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

## Advanced Usage with Local Recipes

Placing `el-get-bundle` macro calls instead of `(el-get 'sync)` in your init
file to explicitly specify which packages should be installed.  The macro
accepts either a simple package name from defined recipes, a package name
with a local recipe definition, a package with initialization code, or
everything together.

Note that if you leave in the `(el-get 'sync)` call, it *must* go
after any recipe defining `el-get-bundle` calls, otherwise el-get
won't know the recipe when it tries to initialize the package.

```lisp
;; Basic setup

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Simple package names
(el-get-bundle yasnippet)
(el-get-bundle color-moccur)

;; Locally defined recipe
(el-get-bundle yaicomplete
  :url "https://github.com/tarao/elisp.git"
  :features yaicomplete)

;; With initialization code
(el-get-bundle zenburn-theme
  :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
  (load-theme 'zenburn t))

```

If a package with a local recipe definition has a recipe file, the
definition overrides that in the recipe file.

There are some syntactic sugars to specify a package name and a recipe source
together.

```lisp
(el-get-bundle tarao/tab-group-el)
;; equivalent to
;; (el-get-bundle tab-group-el :type github :pkgname "tarao/tab-group-el")

(el-get-bundle gist:4468816:pit
;; equivalent to
;; (el-get-bundle pit :type git :url "http://gist.github.com/4468816.git")

(el-get-bundle elpa:undo-tree)
;; equivalent to
;; (el-get-bundle undo-tree :type elpa)
```

Please refer to the *Info* documentation provided with El-Get for the
complete syntax of `el-get-bundle` and recipe definitions.

# Conclusion

Enjoy El-get, enjoy Emacs, have fun with Emacs Lisp, and simplify your Emacs
Setup **today**!
