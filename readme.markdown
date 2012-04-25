El-get allows you to install and manage `elisp` code for Emacs. It supports lots of differents types of sources and is able to *install* them, *update* them and *remove* them, but more importantly it will *init* them for you.  That means it will `require` the *features* you need, `load` the necessary files, set the *Info* paths so that `C-h i` shows the new documentation you now depend on, and finally call your own `:post-init` function for you to setup the extension. Or call it a package.

# Introduction

There are many methods to keep track of your emacs setup.  You can manage it all in a private git repository, you can set up `git submodules` or directly import external repositories.  You can manually retrieve the various packages you wish to track and ensure they are installed on any machine you apply your configuration to.  All of these systems require some degree of manual maintenance, especially if you have packages from various types of locations: github, emacswiki, elpa, privately hosted pages, bzr, cvs, the list goes on.  El-get is designed to simplify this process and allow access to all the various methods of obtaining packages from a single interface.  Every package has a recipe that allows you to locate the original source, and that can be updated if the package is moved.

Whether you are using one machine or many, el-get provides you with a simple interface to your list of installed packages, and the tools to keep them up to date.

# Version Information

`El-get` is currently stable, ready for daily use and packed with extra features that will make life easier.  There are further features that could be added, as always, but they will have the goal of improving user experience.

## Current release

The current stable release is 4.1.

## Version Numbering

Version String are now inspired by how Emacs itself numbers its versions. First is the major version number, then a dot, then the minor version number.  The minor version number is 0 when still developping the next major version.  So 3.0 is a developer release while 3.1 is a stable release.

Please note that this versioning policy has been picked while backing 1.2~dev, so 1.0 was a "stable" release in fact.  Ah, history.

In addition to the version, you can also get the exact git revision by running M-x `el-get-self-checksum`. You should provide this checksum when seeking support or reporting a bug, so that the developers will know exactly which version you are using.

# Installation

El-get is easy to install.  The only requirements to do so successfully are Emacs, git and a connection to the internet that allows you to clone git repositories.

If you do not already have git on your system, you can install it through your package manager if you are using Linux or by downloading it from the [Git Homepage](http://git-scm.com/).

## Stable Branch

To install el-get you can use the *lazy-installer*.  This will not load it on startup or otherwise affect future usage of Emacs.  If you wish to ensure that el-get will be available in future Emacs session please use the code provided in **Basic Setup**.  Using the code below will require an internet connection to start Emacs even if el-get is already installed while the code in the later section will only require a connection if it cannot find an existing installation.

    ;; So the idea is that you copy/paste this code into your *scratch* buffer,
    ;; hit C-j, and you have a working el-get.
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp)))

Evaluating this code after copying it into your `\*scratch\*` buffer by typing `C-j` or `M-x eval-print-last-exp` will retrieve the el-get installation script.  This script will then use git to clone el-get and install it to the default location (`~/.emacs.d/el-get/el-get`).

## Master Branch

The lazy installer above targets the current stable release.  If you would rather use the current development version you must clone the `master` branch by ensuring the variable `el-get-master-branch` exists.

    ;; So the idea is that you copy/paste this code into your *scratch* buffer,
    ;; hit C-j, and you have a working developper edition of el-get.
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (let (el-get-master-branch)
         (goto-char (point-max))
         (eval-print-last-sexp))))

# Setup

El-get requires only a small addition to your init file to be functional.  The provided defaults include settings that will allow you to easily `install`, `update` and `remove` packages that have recipes included without any further configuration.  The **Basic Setup** below includes all that is needed to use el-get, **Setup Customization** lists a few of the variables that can be *customized* when configuring el-get, while **Advanced Setup** looks at including lists of packages to be installed.

## Basic Setup

If you wish to ensure that el-get is available when you load Emacs you can place the following elisp code in your init file.  It will detect if `el-get` is already installed and install it if necessary.

The addition of `(el-get 'sync)` in the code blocks below ensures that any currently *installed* packages will be initialized and any *required* packages will be installed.  This will be explained in further detail in **Calling el-get**.

    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

    (unless (require 'el-get nil t)
      (url-retrieve
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       (lambda (s)
         (goto-char (point-max))
         (eval-print-last-sexp))))

    (el-get 'sync)

And for those who prefer the master branch, please use the code below

    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

    (unless (require 'el-get nil t)
      (url-retrieve
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
       (lambda (s)
         (let (el-get-master-branch)
           (goto-char (point-max))
           (eval-print-last-sexp)))))

    (el-get 'sync)

### Setup Customization

Even though the defaults that are provided by *el-get* provide all that you need to get it working, there may be a reason to manually define certain settings, particularly for portability.  

- **el-get-dir**

 Path where packages are installed.  `~/.emacs.d/el-get/`

- **el-get-install-dir**

 Path for the *el-get* package.  `~/.emacs.d/el-get/el-get`

- **el-get-install-branch**

 If this is set, el-get will be installed using the target `branch`.  This takes precedence over setting `el-get-master-branch` in the installation settings above.

- **el-get-git-install-url**

 Use this to specify your own fork of *el-get* for installation.

- **el-get-recipe-path-elpa**

 This directory stores a local list of *elpa* recipes.  `~/.emacs.d/el-get/el-get/recipes/elpa`

- **el-get-recipe-path-emacswiki**

 As above for *elpa*, this directory stores a local list of *emacswiki* recipes.  `~/.emacs.d/el-get/el-get/recipes/emacswiki/`

## Advanced Setup

Although simply installing and requiring *el-get* is enough to make it usable, it is not a truly portable configuration until you have also defined a list of packages to install.  If you only wish to install packages that have recipes provided for them, you simply need to provide a list of the recipe (package) names.

See **Creating Recipes** for details on how to include additional recipes and **Recipe Customization** to override the defaults of a given recipe.

    ;; local sources
    (setq el-get-sources
          '((:name magit
                   :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

            (:name asciidoc
                   :type elpa
                   :after (lambda ()
                            (autoload 'doc-mode "doc-mode" nil t)
                            (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
                            (add-hook 'doc-mode-hook '(lambda ()
                                                        (turn-on-auto-fill)
                                                        (require 'asciidoc)))))

            (:name lisppaste        :type elpa)
            (:name emacs-goodies-el :type apt-get)))

    (setq my-packages
          (append
           '(cssh el-get switch-window vkill google-maps nxhtml xcscope yasnippet)
           (mapcar 'el-get-source-name el-get-sources)))

    (el-get 'sync my-packages)

So now you have a pretty good documentation of the packages you want installed, where to get them, and how to install them.  Many of the packages you want to install will already have recipes available and only need to be named.  For the advanced methods (such as elpa or apt-get), you basically just need the package name.  When retrieving a package directly from the web through one of the available methods you need to give some more information, such as the URL to clone and the build steps if any.  Then also what features to require and maybe where to find the texinfo documentation of the package, for automatic inclusion into your local Info menu.

The good news is that not only you now have a solid readable description of all that in a central place, but this very description is all (el-get) needs to do its magic.  This command will check that each and every package is installed on your system (in `el-get-dir`) and if that's not the case, it will actually install it.  Then, it will init the packages: that means caring about the load-path, the Info-directory-list (and dir texinfo menu building) the loading of the emacs-lisp files, and finally it will require the features.

# Usage

El-get requires very little interaction with your init file when managing packages.  **Basic Usage** explains how to manage your packages without ever having to touch your init file, while **Advanced Usage** goes into details on how to further customize and manage el-get in a portable way.

Regardless of how you choose to use el-get, the status will still be used to track your packages.  Information about it is included in the section **The status file**.

## Basic usage

### Adding and removing packages

- **M-x el-get-install**

   Will prompt for a package name, with completion, then install it.  It will only propose packages that are not already `installed`.  Any package that you have a recipe for is a candidate.

- **M-x el-get-remove**

   Will prompt for an `installed` package name, with completion, then remove it. Depending on the `type` of the package, this often means simply deleting the directory where the source package lies. Sometime we have to use external tools instead (e.g. `apt-get`). No effort is made to unload the features.

- **M-x el-get-reinstall**

   This is just a shortcut for `el-get-remove` followed by `el-get-install` of the same package. It is primarily useful when a package has changed types, so the normal `el-get-update` process will not work correctly.

### Keeping up to date

- **M-x el-get-self-update**

   Update only one package, `el-get` itself.

- **M-x el-get-update**

   Will prompt for an installed package name, with completion, then update it. This will run the `build` commands and `init` the package again.

- **M-x el-get-update-all**

   Will update all packages that have the `installed` status in your status file.  Before the update you will be prompted for confirmation that you wish to proceed.

   Beware that using this function can lead to hours of settings review: more often than not updating a package requires some adjustments to your setup.  Updating all of them at once will require reviewing almost all your setup.

- **M-x el-get-reload**

   Reload the given package files.  Happens automatically at update time too.

### Viewing available recipes

- **M-x el-get-list-packages**

   Opens a buffer listing all known packages (those for which you have a recipe).  The listing includes the package name, its status (one of "available", "installed", "removed" or "required") and the package description.  The description is a free form text and has not been provided for all recipes.  Please also note that `el-get-emacswiki-refresh` will create recipes omitting the description as of now.

- **M-x el-get-describe**

   Prompt for a package name, with completion, then open an `\*Help\*` window with details about the selected package.  Those include current status, website, description, installation method, full recipe, and buttons to easily install, update or remove the package.

- **M-x el-get-find-recipe-file**

   Will prompt for the name of a package, with completion, then `find-file` its `recipe` file.  If the recipe does not exist, it will create a new recipe file with the appropriate name.

## Advanced Usage

### Calling el-get

In the above sections there have been two cases where the examples finished with a single `(el-get)` call.

When `(el-get)` is called without an attached package list, it only installs packages that are listed as `required` in the status file, and initializes those that are listed as `installed`.

If you wish to provide a list of packages to install or initialize, regardless of whether they are already listed in the status file, you must also specify whether or not you want the work to be done synchronously or not.  This option is discussed in **Sync or async?**

#### Sync or async?

Most often you want `el-get-install` and `el-get-build` to stay out of the way and be /asynchronous/, so that you can continue using Emacs while your new package is getting ready.  But imagine you're starting up Emacs after a `git pull` on the other computer (after a commute, say), and there's some newer packages for this instance to consider installing.

Now you want a synchronous install, right?

So, by default `(el-get)` is asynchronous, but you can ask for it to be sync, or to still be asynchronous but to wait until it finished before to give control back:

    (el-get 'sync)
    (el-get 'wait)

Using `wait` even provides a progress report !

### Package Sources

All packages installed by el-get require some form of a recipe.  Some recipes are included directly in `el-get` so that you only have to include their name in your package list, others must be manually created if you wish to use them.

#### Creating Recipes

See the documentation of the `el-get-sources` variable for details.  Please note that `el-get-sources` is another source location for recipes, adding to your `el-get-recipe-path`.

Note that you can also give a mix of `packages` symbols, `inline recipes` and `source lists` to `el-get` as arguments, and completely bypass the `el-get-sources` variable.

    (el-get 'sync 'package 'name 'list-of-packages-names-or-symbol)

Previously `el-get-sources` was used to generate the list of packages to be managed when calling `(el-get '...)`.  As of 3.1 a change has been included so that `el-get-sources` is now only another source for recipes, and `(el-get '...)` will now only install and initialize known "required" and "installed" packages.

##### Build Commands

Avoid using `make install`, which will usually move files into a "system location."  In our case, you probably just want your package `foo` to be all installed into `~/.emacs.d/el-get/foo`, right? So, no `make install`.

##### Byte Compiling

`el-get` will *byte compile* the elisp for the package when its source definition includes a `:compile` property set to the list of files to byte compile (or to a single file), or all the `.el` files found in the package when there's no `:build` command.

#### Recipe Customization

Should you need some local specific setup, you can provide partial sources that are missing the `:type` property.  Your local properties will be merged with those found in the recipe file before the package is installed.  You can change the `:type` property if necessary, however doing so may cause the recipe to break if associated properties, such as `:url` are not also updated.

### User Package Customization

In addition to setting an `:after` property when customizing a recipe, you can provide files named `init-package.el` (support for literate `.org` files is in progress) stored in the directory defined by `el-get-user-package-directory`.  

Any such named file will get automatically loaded by `el-get` at `init` time, if it exists, or once the package is loaded if lazy initialization is set.

### Hooks

`el-get` offers a variety of specific hooks (read the source), and two general purposes hooks facilities: `el-get-post-install-hooks` and `el-get-post-update-hooks`, called with the package name as argument.

## The status file

The status of each package is tracked into `~/.emacs.d/el-get/.status.el` (by default) and can get the values `required`, `installed` or `removed`.  The status file also keeps track of the recipe used to install the package, this ensures the package can always be initialized even if the recipe is no longer available.

## Additional recipe sources

In addition to the bundled recipes, there are two recipe sources provided that simply need to be updated if you wish to use them.  

Elisp scripts hosted on the [EmacsWiki](http://emacswiki.org) can be retrieved using `M-x el-get-emacswiki-refresh`.  The resulting recipe files will be stored in the directory defined by `el-get-recipe-path-emacswiki`.  You will the recipes are updated.

Packages managed by ELPA are retrieved and updated using `M-x el-get-elpa-build-local-recipies`.  The recipe files are stored in the directory defined by `el-get-recipe-path-elpa`.

## Additional commands and useful functions

- **M-x el-get-checksum**

   Will prompt for the name of an installed package, with complement, then compute its checksum if the package type supports that feature.  The checksum is added to the kill-ring so that you're ready to yank it into your `el-get-sources` :checksum property if you want to.

- **M-x el-get-cd**

   Will prompt for an `installed` package name, with completion, then open its directory with dired.

- **el-get-package-types-alist (statuses &rest types)**

   Return an alist of package names that are of given types. Only consider packages whose status is `member' of STATUSES, which defaults to installed, required and removed.

              ELISP> (el-get-package-types-alist "installed" 'cvs 'emacswiki)
              ((emacs-w3m . cvs)
               (rect-mark . emacswiki)
               (icomplete+ . emacswiki)
               (php-mode-improved . emacswiki)
               (rainbow-delimiters . emacswiki)
               (goto-last-change . emacswiki)
               (emacs-goodies-el . cvs))

- **el-get-extra-packages (&rest packages)**

   Return installed or required packages that are not in given package list.

              ELISP> (el-get-extra-packages dim-packages)
              ((verbiste "installed")
               (package "installed"))

# Extending el-get

Extending el-get is most easily done on two fronts: providing new or updated recipes for packages and providing new methods for packages stored in currently unsupported formats.

## Recipes

El-get recipes follow the format described above in **Creating Recipes**, however they are stored as `recipe-name.rcp`.  The command `el-get-make-recipes` will convert any local recipes stored in `el-get-sources` to the appropriate recipe files.  They can also be created or modified by using `el-get-find-recipe-file`.

## Methods

Please see the documentation for the `el-get-methods` and provide a patch!

When adding a new method, make sure to include the appropriate `(require 'method-name)` in `el-get-methods.el` to ensure it is available when el-get is loaded.

Adding `bzr` support for example was only about writing 2 functions, mostly using copy paste. Here's the patch: https://github.com/dimitri/el-get/commit/63e9018102bdeb7b6d9136db231adcd983087217#L0R437

# Upgrade Notes

## TODO Upgrading to 4.1

Pending conclusion in [#722](https://github.com/dimitri/el-get/issues/722)

Currently there is no upgrade path from 3.1 to 4.1, you will need to reinstall el-get manually although it will then retrieve your currently installed packages (Without guarantee that the new recipes will comply).
