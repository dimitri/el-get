;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Version: 3.1
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs svn darcs hg
;;           apt-get fink pacman http http-tar emacswiki
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.asciidoc file from the same distribution

;;; Commentary:
;;
;; Version String are now inspired by how Emacs itself numbers its version.
;; First is the major version number, then a dot, then the minor version
;; number.  The minor version number is 0 when still developping the next
;; major version.
;;
;; So 2.0 is a developer release while 2.1 will be the next stable release.
;;
;; Please note that this versioning policy has been picked while backing
;; 1.2~dev, so 1.0 was a "stable" release in fact.  Ah, history.

;;; Change Log:
;;
;;  3.1 - 2011-09-15 - Get a fix
;;
;;   - support for package dependencies
;;   - rely on package status for `el-get' to install and init them
;;   - M-x el-get-list-packages
;;   - support for :branch in git
;;   - new recipes, galore
;;   - bug fixes, byte compiling, windows compatibility, etc
;;   - recipe files are now *.rcp rather than *.el (el still supported)
;;   - el-get-user-package-directory allows to setup init-<package>.el files
;;   - remove M-x el-get-sync, now obsolete
;;
;;  2.2 - 2011-05-26 - Fix the merge
;;
;;   - Revert changes introduced by a merge done by mistake
;;
;;  2.1 - 2011-05-25 - Still growing, getting lazy
;;
;;   - Add support for autoloads, per Dave Abrahams
;;   - fix 'wait support for http (using sync retrieval)
;;   - code cleanup per Dave Abrahams, lots of it
;;   - add function M-x el-get-update-all
;;   - Implement M-x el-get-make-recipes
;;   - byte-compile at build time rather than at init time
;;   - and use a "clean room" external emacs -Q for byte compiling
;;   - allow to skip autoloads either globally or per-package
;;   - better checks and errors for commands used when installing packages
;;   - register `el-get-sources' against the custom interface
;;   - el-get will now accept a list of sources to install or init
;;   - open el-get-{install,init,remove,update} from `el-get-sources' only
;;   - add :prepare and :post-init and :lazy, and `el-get-is-lazy'
;;   - add support for :repo for ELPA packages
;;
;;  1.1 - 2010-12-20 - Nobody's testing until the release
;;
;;   - Adapt to package.el from Emacs24 by using relative symlinks to ELPA
;;     packages ((package-user-dir) is "~/.emacs.d/elpa" now, so needs to
;;     get expanded at least)
;;   - Allow to bypass byte compiling entirely with a single global var
;;   - Have http local file default to something sane, not package.el
;;   - Implement support for svn and darcs too
;;   - Still more recipes
;;   - Add support for the `pacman' package manager (ARCH Linux)
;;   - Add support for mercurial
;;   - (el-get 'sync) now really means synchronous, and serialized too
;;   - el-get-start-process-list implements :sync, defaults to nil (async)
;;   - Implement a :localname package property to help with some kind of URLs
;;   - Add el-get-post-init-hooks
;;   - Allow build commands to be evaluated, hence using some emacs variables
;;   - Finaly walk the extra mile and remove "required" packages at install
;;   - Implement support for the "Do you want to continue" apt-get prompt
;;   - implement :before user defined function to run before init
;;
;;  1.0 - 2010-10-07 - Can I haz your recipes?
;;
;;   - Implement el-get recipes so that el-get-sources can be a simple list
;;     of symbols. Now that there's an authoritative git repository, where
;;     to share the recipes is easy.
;;   - Add support for emacswiki directly, save from having to enter the URL
;;   - Implement package status on-disk saving so that installing over a
;;     previously failed install is in theory possible. Currently `el-get'
;;     will refrain from removing your package automatically, though.
;;   - Fix ELPA remove method, adding a "removed" state too.
;;   - Implement CVS login support.
;;   - Add lots of recipes
;;   - Add support for `system-type' specific build commands
;;   - Byte compile files from the load-path entries or :compile files
;;   - Implement support for git submodules with the command
;;     `git submodule update --init --recursive`
;;   - Add catch-all post-install and post-update hooks
;;   - Add desktop notification on install/update.
;;
;;  0.9 - 2010-08-24 - build me a shell
;;
;;   - have `el-get-build' use start-process-shell-command so that you can
;;     pass-in shell commands. Beware of poor command argument "parsing"
;;     though, done with a simple `split-string'.
;;
;;  0.8 - 2010-08-23 - listen to the users
;;
;;   - implement :after user defined function to run at the end of init
;;   - add CVS support (no login support)
;;   - improve el-get-build to use async building
;;   - fix el-get-update doing so
;;
;;  0.7 - 2010-08-23 - archive
;;
;;   - http support is extended to `tar' archives, via the http-tar type
;;
;;  0.6 - 2010-08-12 - towards a stable version
;;
;;   - fix when asynchronous http support call post-install-fun
;;   - fix el-get-remove calling convention
;;   - add support for bzr, thanks to Kevin Fletcher
;;
;;  0.5 - 2010-08-06 - release early, fix often
;;
;;   - fix apt-get and fink install hooks to call el-get-dpkg-symlink
;;   - fix elpa and http support to follow the new call convention
;;   - use asynchronous url-retrieve facility so that http is async too
;;
;;  0.4 - 2010-08-04 - foxy release
;;
;;   - support asynchronous processes for system commands
;;       apt-get, fink, git and git-svn are run in background
;;   - support `sudo' password prompts (classic and ubuntu variants)
;;   - fix fink support
;;   - ELPA support is an option so that you can install ELPA from el-get
;;   - implement el-get-rmdir

;;; Code:

(require 'dired)
(require 'package nil t) ; that's ELPA, but you can use el-get to install it
(require 'cl)            ; needed for `remove-duplicates'
(require 'simple)        ; needed for `apply-partially'
(require 'bytecomp)
(require 'autoload)
(require 'help-mode)     ; byte-compiling needs to know about xref-type buttons

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defconst el-get-version "3.1" "el-get version number")

(defcustom el-get-post-init-hooks nil
  "Hooks to run after a package init.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-install-hooks nil
  "Hooks to run after installing a package.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-update-hooks nil
  "Hooks to run after updating a package.
Each hook is a unary function accepting a package"
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-error-hooks nil
  "Hooks to run after package installation fails.
Each hook is a binay function accepting a package and error data"
  :group 'el-get
  :type 'hook)

(defcustom el-get-byte-compile t
  "Whether or not to byte-compile packages. Can be used to
disable byte-compilation globally, unless this process is not
controlled by `el-get' itself.

The cases when `el-get' loses control are with \"advanced\"
packaging systems (apt-get, fink, pacman, elpa) or when the
recipe contains a :build rule (using a Makefile for example)."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-verbose nil
  "Non-nil means print messages describing progress of el-get even for fast operations."
  :group 'el-get
  :type 'boolean)

(defun el-get-verbose-message (format &rest arguments)
  (when el-get-verbose (apply 'message format arguments)))

(defcustom el-get-byte-compile-at-init nil
  "Whether or not to byte-compile packages at init.

Turn this to t if you happen to update your packages from under
`el-get', e.g. doing `cd el-get-dir/package && git pull`
directly."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-generate-autoloads t
  "Whether or not to generate autoloads for packages. Can be used
to disable autoloads globally."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-is-lazy nil
  "Whether or not to defer evaluation of :post-init and :after
functions until libraries are required.  Will also have el-get
skip the :load and :features properties when set.  See :lazy to
force their evaluation on some packages only."
  :group 'el-get
  :type 'boolean)

(defcustom el-get-git-clone-hook nil
  "Hook run after git clone."
  :group 'el-get
  :type 'hook)

(defcustom el-get-git-svn-clone-hook nil
  "Hook run after git svn clone."
  :group 'el-get
  :type 'hook)

(defcustom el-get-bzr-branch-hook nil
  "Hook run after bzr branch."
  :group 'el-get
  :type 'hook)

(defcustom el-get-cvs-checkout-hook nil
  "Hook run after cvs checkout."
  :group 'el-get
  :type 'hook)

(defcustom el-get-svn-checkout-hook nil
  "Hook run after svn checkout."
  :group 'el-get
  :type 'hook)

(defcustom el-get-darcs-get-hook nil
  "Hook run after darcs get."
  :group 'el-get
  :type 'hook)

(defcustom el-get-apt-get-install-hook nil
  "Hook run after apt-get install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-apt-get-remove-hook nil
  "Hook run after apt-get remove."
  :group 'el-get
  :type 'hook)

(defcustom el-get-fink-install-hook nil
  "Hook run after fink install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-fink-remove-hook nil
  "Hook run after fink remove."
  :group 'el-get
  :type 'hook)

(defcustom el-get-elpa-install-hook nil
  "Hook run after ELPA package install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-elpa-remove-hook nil
  "Hook run after ELPA package remove."
  :group 'el-get
  :type 'hook)

(defcustom el-get-http-install-hook nil
  "Hook run after http retrieve."
  :group 'el-get
  :type 'hook)

(defcustom el-get-http-tar-install-hook nil
  "Hook run after http-tar package install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-pacman-install-hook nil
  "Hook run after pacman install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-pacman-remove-hook nil
  "Hook run after pacman remove."
  :group 'el-get
  :type 'hook)

(defcustom el-get-hg-clone-hook nil
  "Hook run after hg clone."
  :group 'el-get
  :type 'hook)

(defcustom el-get-methods
  '(:git     (:install el-get-git-clone
		       :install-hook el-get-git-clone-hook
		       :update el-get-git-pull
		       :remove el-get-rmdir)
    :emacsmirror (:install el-get-emacsmirror-clone
		       :install-hook el-get-git-clone-hook
		       :update el-get-git-pull
		       :remove el-get-rmdir)
    :git-svn (:install el-get-git-svn-clone
		       :install-hook el-get-git-svn-clone-hook
		       :update el-get-git-svn-update
		       :remove el-get-rmdir)
    :bzr     (:install el-get-bzr-branch
		       :install-hook el-get-bzr-branch-hook
		       :update el-get-bzr-pull
		       :remove el-get-rmdir)
    :svn     (:install el-get-svn-checkout
		       :install-hook el-get-svn-checkout-hook
		       :update el-get-svn-update
		       :remove el-get-rmdir)
    :cvs     (:install el-get-cvs-checkout
		       :install-hook el-get-cvs-checkout-hook
		       :update el-get-cvs-update
		       :remove el-get-rmdir)
    :darcs   (:install el-get-darcs-get
		       :install-hook el-get-darcs-get-hook
		       :update el-get-darcs-pull
		       :remove el-get-rmdir)
    :apt-get (:install el-get-apt-get-install
		       :install-hook el-get-apt-get-install-hook
		       :update el-get-apt-get-install
		       :remove el-get-apt-get-remove
		       :remove-hook el-get-apt-get-remove-hook)
    :fink    (:install el-get-fink-install
		       :install-hook el-get-fink-install-hook
		       :update el-get-fink-install
		       :remove el-get-fink-remove
		       :remove-hook el-get-fink-remove-hook)
    :elpa    (:install el-get-elpa-install
		       :install-hook el-get-elpa-install-hook
		       :update el-get-elpa-update
		       :remove el-get-elpa-remove
		       :remove-hook el-get-elpa-remove-hook)
    :http    (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
		       :remove el-get-rmdir)
    :ftp     (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
		       :remove el-get-rmdir)
    :emacswiki (:install el-get-emacswiki-install
		       :install-hook el-get-http-install-hook
		       :update el-get-emacswiki-install
		       :remove el-get-rmdir)
    :http-tar (:install el-get-http-tar-install
		       :install-hook el-get-http-tar-install-hook
		       :update el-get-http-tar-install
		       :remove el-get-rmdir)
    :pacman   (:install el-get-pacman-install
                        :install-hook el-get-pacman-install-hook
                        :update el-get-pacman-install
                        :remove el-get-pacman-remove
                        :remove-hook el-get-pacman-remove-hook)
    :hg       (:install el-get-hg-clone
                        :install-hook el-get-hg-clone-hook
                        :update el-get-hg-pull
                        :remove el-get-rmdir))
  "Register methods that el-get can use to fetch and update a given package.

The methods list is a PLIST, each entry has a method name
property which value is another PLIST, which must contain values
for :install, :install-hook, :update and :remove
properties. Those should be the elisp functions to call for doing
the named package action in the given method."
  :type '(repeat (cons symbol function))
  :group 'el-get)

(defconst el-get-script (or load-file-name buffer-file-name))

(defcustom el-get-dir "~/.emacs.d/el-get/"
  "Path where to install the packages."
  :group 'el-get
  :type 'directory)

(defcustom el-get-recipe-path-emacswiki
  (concat (file-name-directory el-get-dir) "el-get/recipes/emacswiki/")
  "Define where to keep a local copy of emacswiki recipes"
  :group 'el-get
  :type 'directory)

(defcustom el-get-recipe-path
  (list (concat (file-name-directory el-get-script) "recipes")
	el-get-recipe-path-emacswiki)
  "Define where to look for the recipes, that's a list of directories"
  :group 'el-get
  :type '(repeat (directory)))

(defcustom el-get-user-package-directory nil
  "Define where to look for init-pkgname.el configurations. Disabled if nil."
  :group 'el-get
  :type '(choice (const :tag "Off" nil) directory))

(defun el-get-load-package-user-init-file (package)
  "Load the user init file for PACKAGE, called init-package.el
and to be found in `el-get-user-package-directory'.  Do nothing
when this custom is nil."
  (when el-get-user-package-directory
    (let* ((init-file-name    (concat "init-" package ".el"))
	   (package-init-file
	    (expand-file-name init-file-name el-get-user-package-directory)))
      (el-get-verbose-message "el-get: load %S" package-init-file)
      (load package-init-file 'noerror))))

(defun el-get-recipe-dirs ()
  "Return the elements of el-get-recipe-path that actually exist.

Used to avoid errors when exploring the path for recipes"
  (reduce (lambda (dir result)
            (if (file-directory-p dir) (cons dir result) result))
          el-get-recipe-path :from-end t :initial-value nil))

;; recipe files are elisp data, you can't byte-compile or eval them on their
;; own, but having elisp indenting and colors make sense
(eval-and-compile
  (add-to-list 'auto-mode-alist '("\\.rcp\\'" . emacs-lisp-mode)))

(defcustom el-get-status-file
  (concat (file-name-as-directory el-get-dir) ".status.el")
  "Define where to store and read the package statuses")

(defvar el-get-autoload-file
  (concat (file-name-as-directory el-get-dir) ".loaddefs.el")
  "Where generated autoloads are saved")

(defvar el-get-outdated-autoloads nil
  "List of package names whose autoloads are outdated")

(defvar el-get-emacs (concat invocation-directory invocation-name)
  "Where to find the currently running emacs, a facility for :build commands")

(defcustom el-get-apt-get (executable-find "apt-get")
  "The apt-get executable."
  :group 'el-get
  :type 'file)

(defcustom el-get-apt-get-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'directory)

(defcustom el-get-fink (executable-find "fink")
  "The fink executable."
  :group 'el-get
  :type 'directory)

(defcustom el-get-svn (executable-find "svn")
  "The svn executable."
  :group 'el-get
  :type 'file)

(defcustom el-get-fink-base "/sw/share/doc"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'file)

(defcustom el-get-emacswiki-base-url
  "http://www.emacswiki.org/emacs/download/%s.el"
  "The base URL where to fetch :emacswiki packages"
  :group 'el-get
  :type 'string)

(defcustom el-get-emacswiki-elisp-index-url
  "http://www.emacswiki.org/cgi-bin/wiki?action=index;match=%5C.(el%7Ctar)(%5C.gz)%3F%24"
  "The emacswiki index URL of elisp pages"
  :group 'el-get
  :type 'string)

(defcustom el-get-emacswiki-elisp-index-base-url
  "http://www.emacswiki.org/emacs/"
  "The emacswiki base URL used in the index"
  :group 'el-get
  :type 'string)

(defcustom el-get-emacsmirror-base-url
  "http://github.com/emacsmirror/%s.git"
  "The base URL where to fetch :emacsmirror packages.  Consider using
\"git://github.com/emacsmirror/%s.git\"."
  :group 'el-get
  :type '(choice (const "http://github.com/emacsmirror/%s.git")
                 (const "https://github.com/emacsmirror/%s.git")
                 (const "git://github.com/emacsmirror/%s.git")
                 string))

(defcustom el-get-pacman-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'directory)

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
				(executable-find "install-info")))

;; we support notifications on darwin too, thanks to growlnotify
(defcustom el-get-growl-notify "/usr/local/bin/growlnotify"
  "Absolute path of the growlnotify tool"
  :group 'el-get
  :type 'file)

(defconst el-get-build-recipe-body
  '(choice :tag "Format"

           (repeat :tag "List of shell commands"
                    (string :doc "Note: arguments will not be shell-quoted.
Choose `Evaluated expression' format for a more portable recipe" :format "%v%h"))
           (sexp :tag "Evaluated expression" :format "%t: %v%h"
                 :value `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make") ("make" ("install")))

                 :doc "Evaluation should yield a list of lists.
Each sub-list, representing a single shell command, is expected to have
strings and/or lists as elements, sub-sub-lists can have string and/or
list elements, and so on.  Each sub-list will be \"flattened\" to produce
a list of strings, each of which will be `shell-quote-argument'ed before
being sent to the underlying shell."
                 )
           ))


;;
;; Support for tracking package states
;;
(defvar el-get-pkg-state
  (make-hash-table)
  "A hash mapping el-get package name symbols to their installation states")

(defun el-get-package-state (package)
  "Return the installation state of PACKAGE.

- nil indicates that installation of the package has not been requested
- 'installing indicates that the package's installation is in progress
- 'init indicates that the package has been initialized
- ('error . <data>) indicates that there was an installation error"
  (gethash (el-get-as-symbol package) el-get-pkg-state))

(defun el-get-currently-installing-p (package)
  (eq (el-get-package-state package) 'installing))

(defun el-get-currently-installing-packages ()
  "Return the packages that are currently installing"
  (loop
   for pkg being the hash-keys of el-get-pkg-state
   if (el-get-currently-installing-p pkg)
   collect pkg))

(defun el-get-set-package-state (package state)
  "Set the installation state of PACKAGE to STATE"
  (puthash (el-get-as-symbol package) state el-get-pkg-state))

(defun el-get-mark-initialized (package)
  "Record the fact that the given PACKAGE has been initialized."
  (el-get-set-package-state package 'init))
(add-hook 'el-get-post-init-hooks 'el-get-mark-initialized)

(defun el-get-mark-removed (package)
  "Record the fact that the given PACKAGE has been initialized."
  (el-get-set-package-state package nil))
(add-hook 'el-get-post-remove-hooks 'el-get-mark-removed)

(defun el-get-mark-failed (package info)
  "Record the fact that the given PACKAGE has failed to install
for reasons described in INFO."
  (el-get-verbose-message "el-get-mark-failed: %s %s" package info)
  (el-get-set-package-state package `(error ,info)))
(add-hook 'el-get-post-error-hooks 'el-get-mark-failed)


;;
;; "Fuzzy" data structure handling
;;
;; In el-get-sources, single elements are often allowed instead of a
;; list, and strings and symbols are often interchangeable.
;; Presumably it's easier for users who don't use the customization
;; interface to write such structures as raw elisp.
;;
;;;  "Fuzzy" data structure conversion utilities
(defun el-get-as-string (symbol-or-string)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp symbol-or-string) symbol-or-string
    (symbol-name symbol-or-string)))

(defun el-get-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
      (intern string-or-symbol)))

(defun el-get-as-list (element-or-list)
  "If ELEMENT-OR-LIST is already a list, return it.  Otherwise
returning a list that contains it (and only it)."
  (if (listp element-or-list) element-or-list
      (list element-or-list)))

;;; "Fuzzy" data structure customization widgets
(defun el-get-repeat-value-to-internal (widget element-or-list)
  (el-get-as-list element-or-list))

(defun el-get-repeat-match (widget value)
  (widget-editable-list-match widget (el-get-repeat-value-to-internal widget value)))

(define-widget 'el-get-repeat 'repeat
  "A variable length list of non-lists that can also be represented as a single element"
  :value-to-internal 'el-get-repeat-value-to-internal
  :match 'el-get-repeat-match)

(defun el-get-symbol-match (widget value)
  (or (symbolp value) (stringp value)))

(define-widget 'el-get-symbol 'symbol
  "A string or a symbol, rendered as a symbol"
  :match 'el-get-symbol-match
)
;;; END "Fuzzy" data structure support



(defun el-get-source-name (source)
  "Return the package name (stringp) given an `el-get-sources'
entry."
  (if (symbolp source) (symbol-name source)
    (format "%s" (plist-get source :name))))

(defcustom el-get-sources nil
  "Additional package recipes

Each entry is a PLIST where the following properties are
supported.

If your property list is missing the :type property, then it's
merged with the recipe one, so that you can override any
definition provided by `el-get' recipes locally.

:name

    The name of the package. It can be different from the name of
    the directory where the package is stored (after a `git
    clone' for example, in which case a symlink will be created.

:depends

    A single package name, or a list of package names, on which
    the package depends.  All of a packages dependencies will be
    installed before the package is installed.

:pkgname

    The name of the package for the underlying package management
    system (`apt-get', `fink' or `pacman', also supported by
    `emacsmirror'), which can be different from the Emacs package
    name.

:type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git', `emacsmirror', `git-svn', `bzr' `svn',
    `cvs', `darcs', `fink', `ftp', `emacswiki', `http-tar', `pacman',
    `hg' and `http'. You can easily support your own types here, 
    see the variable `el-get-methods'.

:branch

    Which branch to fetch when using `git'.  Also supported in
    the installer in `el-get-install'.

:url

    Where to fetch the package, only meaningful for `git' and `http' types.

:build

    Your build recipe, a list.

    A recipe R whose `car' is not a string will be replaced
    by (eval R).

    Then, each element of the recipe will be interpreted as
    a command:

    * If the element is a string, it will be interpreted directly
      by the shell.

    * Otherwise, if it is a list, any list sub-elements will be
      recursively \"flattened\" (see `el-get-flatten').  The
      resulting strings will be interpreted as individual shell
      arguments, appropriately quoted.

:build/system-type

    Your specific build recipe for a given `system-type' gets
    there and looks like :build.

:load-path

    A directory or a list of directories you want `el-get' to add
    to your `load-path'. Those directories are relative to where
    the package gets installed.

:compile

    Allow to restrict what to byte-compile: by default, `el-get'
    will compile all elisp files in the :load-path directories,
    unless a :build command exists for the package source. Given
    a :compile property, `el-get' will only byte-compile those
    given files, directories or filename-regexpes in the property
    value. This property can be a `listp' or a `stringp' if you
    want to compile only one of those.

:info

    This string allows you to setup a directory where to find a
    'package.info' file, or a path/to/whatever.info file. It will
    even run `ginstall-info' for you to create the `dir' entry so
    that C-h i will be able to list the newly installed
    documentation. Note that you might need to kill (C-x k) your
    info buffer then C-h i again to be able to see the new menu
    entry.

:load

    List of files to load, or a single file to load after having
    installed the source but before `require'ing its features.

:features

    List of features el-get will `require' for you.

:autoloads

    Control whether el-get should generate autoloads for this
    package. Setting this to nil prevents el-get from generating
    autoloads for the package. Default is t. Setting this to a
    string or a list of string will load the named autoload
    files.

:library

    When using :after but not using :features, :library allows to
    set the library against which to register the :after function
    against `eval-after-load'.  It defaults to either :pkgname
    or :package, in this order.  See also `el-get-eval-after-load'.

:options

    Currently used by http-tar and cvs support.

    When using http-tar, it allows you to give the tar options
    you want to use. Typically would be \"xzf\", but you might
    want to choose \"xjf\" for handling .tar.bz files e.g.

    When using CVS, when it's set to \"login\", `el-get' will
    first issue a `cvs login' against the server, asking you
    interactively (in the minibuffer) any password you might to
    enter, and only then it will run the `cvs checkout' command.

:module

    Currently only used by the `cvs' support, allow you to
    configure the module you want to checkout in the given URL.

:repo

    Only used by the `elpa' support, a cons cell with the
    form (NAME . URL), as in `package-archives'.  If the package
    source only specifies a URL, the URL will be used for NAME as
    well.

:prepare

    Intended for use from recipes, it will run once both the
    `Info-directory-list' and the `load-path' variables have been
    taken care of, but before any further action from
    `el-get-init'.

:before

    A pre-init function to run once before `el-get-init' calls
    `load' and `require'.  It gets to run with `load-path'
    already set, and after :prepare has been called.  It's not
    intended for use from recipes.

:post-init

    Intended for use from recipes.  This function is registered
    for `eval-after-load' against the recipe library by
    `el-get-init' once the :load and :features have been setup.

:after

    A function to register for `eval-after-load' against the
    recipe library, after :post-init, and after per-package
    user-init-file (see `el-get-user-package-directory').  That's not
    intended for recipe use.

:lazy

    Default to nil.  Allows to override `el-get-is-lazy' per
    package.

:localname

    Currently only used by both `http' and `ftp' supports, allows
    to specify the target name of the downloaded file.

    This option is useful if the package should be retrieved using
    a presentation interface (such as as web SCM tool).

    For example, destination should be set to \"package.el\" if
    the package url has the following scheme:

   \"http://www.example.com/show-as-text?file=path/package.el\"

:website

    The website of the project.

:description

    A short description of the project.

"

  :type
  `(repeat
    (choice
     :tag "Entry"
     :value (:name "")
     (el-get-symbol :tag "Name of EL-Get Package")
     (list
      :tag "Full Recipe (or Recipe Override)"
      (group :inline t :tag "EL-Get Package Name" :format "%t: %v"
             (const :format "" :name) (el-get-symbol :format "%v"))
      (set
       :inline t :format "%v\n"
       (group
        :inline t  (const :format "" :depends)
        (el-get-repeat
         :tag "Names of packages on which this one depends" el-get-symbol))
       (group
        :inline t :format "%t: %v%h"
        :tag "Underlying Package Name"
        :doc "When there is an underlying package manager (e.g. `apt')
this is the name to fetch in that system"
        (const :format "" :pkgname) (string :format "%v"))

       (group
        :inline t :tag "Type" :format "%t: %v%h"
        :doc "(If omitted, this recipe provides overrides for one in recipes/)"
        (const :format "" :type)
        ,(append '(choice :value emacswiki :format "%[Value Menu%] %v"
                          )
                 ;; A sorted list of method names
                 (sort
                  (reduce
                   (lambda (r e)
                     (if (symbolp e)
                         (cons
                          (list 'const
                                (intern (substring (prin1-to-string e) 1)))
                          r)
                       r))
                   el-get-methods
                   :initial-value nil)
                  (lambda (x y)
                    (string< (prin1-to-string (cadr x))
                             (prin1-to-string (cadr y)))))))

       (group :inline t :format "Source URL: %v"
              (const :format "" :url) (string :format "%v"))
       (group :inline t :format "Package Website: %v"
              (const :format "" :website) (string :format "%v"))
       (group :inline t :format "Description: %v"
              (const :format "" :description) (string :format "%v"))
       (group :inline t :format "General Build Recipe\n%v"
              (const :format "" :build) ,el-get-build-recipe-body)
       (group :inline t  (const :format "" :load-path)
              (el-get-repeat
               :tag "Subdirectories to add to load-path" directory))
       (group :inline t  (const :format "" :compile)
              (el-get-repeat
               :tag "File/directory regexps to compile" regexp))
       (group :inline t :format "%v" (const :format "" :info)
              (string :tag "Path to .info file or to its directory"))
       (group :inline t (const :format "" :load)
              (el-get-repeat :tag "Relative paths to force-load" string))
       (group :inline t :format "%v" (const :format "" :features)
              (repeat :tag "Features to `require'" el-get-symbol))
       (group :inline t :format "Autoloads: %v" :value (:autoloads t)
              (const :format "" :autoloads)
              (choice
               :tag "Type"
               (boolean :format "generation %[Toggle%] %v\n")
               (el-get-repeat
                :tag "Relative paths to force-load" string)))
       (group :inline t :format "Options (`http-tar' and `cvs' only): %v"
              (const :format "" :options) (string :format "%v"))
       (group :inline t :format "CVS Module: %v"
              (const :format "" :module)
              (string :format "%v"))
       (group :inline t :format "`Prepare' Function: %v"
              (const :format "" :prepare) (function :format "%v"))
       (group :inline t :format "`Post-Init' Function: %v"
              (const :format "" :post-init) (function :format "%v"))
       (group :inline t
              :format "Name of downloaded file (`http' and `ftp' only): %v"
              (const :format "" :localname) (string :format "%v"))
       (group :inline t :format "Lazy: %v"  :value (:lazy t)
              (const :format "" :lazy) (boolean :format "%[Toggle%] %v\n"))
       (group :inline t
              :format "Repository specification (`elpa' only): %v"
              (const :format "" :repo)
              (cons :format "\n%v"
                    (string :tag "Name")
                    (string :tag "URL")))
       (group :inline t
              :format "`Before' Function (`Prepare' recommended instead): %v"
              (const :format "" :before) (function :format "%v"))
       (group :inline t
              :format "`After' Function (`Post-Init' recommended instead): %v"
              (const :format "" :after) (function :format "%v")))
      (repeat
       :inline t :tag "System-Specific Build Recipes"
       (group :inline t
              (symbol :value ,(concat ":build/"
                                      (prin1-to-string system-type))
                      :format "Build Tag: %v%h"
                      :doc "Must be of the form `:build/<system-type>',
where `<system-type>' is the value of `system-type' on
platforms where this recipe should apply"
                      )
              ,el-get-build-recipe-body))))))


;;
;; Some tools
;;
(defun el-get-flatten (arg)
  "Return a version of ARG as a one-level list

 (el-get-flatten 'x) => '(x)
 (el-get-flatten '(a (b c (d)) e)) => '(a b c d e)"
  (if (listp arg)
      (apply 'append (mapcar 'el-get-flatten arg))
    (list arg)))

(defun el-get-load-path (package)
  "Return the list of absolute directory names to be added to
`load-path' by the named PACKAGE."
  (let* ((source   (el-get-package-def package))
	 (el-path  (el-get-flatten (or (plist-get source :load-path) ".")))
         (pkg-dir (el-get-package-directory package)))
    (mapcar (lambda (p) (expand-file-name p pkg-dir)) el-path)))

(defun el-get-method (method-name action)
  "Return the function to call for doing action (e.g. install) in
given method."
  (let* ((method  (intern (concat ":" (format "%s" method-name))))
	 (actions (plist-get el-get-methods method)))
    (plist-get actions action)))

(defun el-get-check-init ()
  "Check that we can run el-get."
  (unless (file-directory-p el-get-dir)
    (make-directory el-get-dir)))

(defun el-get-package-directory (package)
  "Return the absolute directory name of the named PACKAGE."
  (file-name-as-directory
   (expand-file-name package (expand-file-name el-get-dir))))

(defun el-get-add-path-to-list (package list path)
  "(add-to-list LIST PATH) checking for path existence within
given package directory."
  (let* ((pdir     (el-get-package-directory package))
	 (fullpath (expand-file-name (or path ".") pdir)))
    (unless (file-directory-p fullpath)
      (error "el-get could not find directory `%s' for package %s, at %s"
	     path package fullpath))
    (add-to-list list fullpath)))

(defun el-get-package-exists-p (package)
  "Return true only when the given package name is either a
directory or a symlink in el-get-dir."
  (let ((pdir (el-get-package-directory package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
	(file-symlink-p   pdir))))

;;
;; generic one-shot event support
;;
(defvar el-get-generic-event-tasks (make-hash-table :test 'equal)
  "A hash mapping event triggers to lists of functions to be called")

(defun el-get-generic-event-occurred (event &optional data)
  "Fire all tasks added for the given EVENT (a hash key), passing DATA."
  (let (tasks)
    (while (setq tasks (gethash event el-get-generic-event-tasks))
      (puthash event (cdr tasks) el-get-generic-event-tasks)
      (ignore-errors (funcall (car tasks) data)))))

(defun el-get-add-generic-event-task (event task)
  "Set up TASK to be called when EVENT (a hash key) occurs."
  (puthash event (cons task (gethash event el-get-generic-event-tasks))
           el-get-generic-event-tasks))

(defun el-get-clear-generic-event-tasks (event)
  "Clear all tasks waiting on EVENT (a hash key)"
  (remhash event el-get-generic-event-tasks))


;;
;; fire events for completion of el-get's init, install, and update
;; phases (and for errors).
;;
(defun el-get-event-id (package action)
  (list (el-get-as-symbol package) (intern (format "el-get-%s" action))))

(defun el-get-event-occurred (package action &optional data)
  "Handle the completion of ACTION on PACKAGE (both symbols),
passing DATA"
  ;; If this action finalizes the package state, first cancel other
  ;; final actions
  (let* ((final-actions '(init error))
         (found (position action final-actions)))
    (when found
      (el-get-clear-generic-event-tasks
       (el-get-event-id package (elt final-actions (- 1 found))))))
  ;; Now fire off the generic event
  (el-get-generic-event-occurred (el-get-event-id package action) data))

;; Install hooks that generate events
(dolist (action '(init install update error))
  (add-hook (intern (format "el-get-post-%s-hooks" action))
            `(lambda (p &optional data) (el-get-event-occurred p ',action data))))

(defun el-get-dependencies (package)
  "Return the list of packages (as symbols) on which PACKAGE (a
symbol) depends"
  (let* ((source (el-get-package-def (symbol-name package)))
	 (method (el-get-package-method source))
         (deps (el-get-as-list (plist-get source :depends))))
    ;; Make sure all elpa packages depend on the package `package'.
    ;; The package `package' is an elpa package, though, so exclude
    ;; it to avoid a circular dependency.
    (if (and (not (eq package 'package)) (eq method 'elpa))
        (cons 'package deps)
      deps)))

(defun el-get-package-initialized-p (package)
  (eq (el-get-package-state package) 'init))

(defun el-get-demand1 (package)
  "Install, if necessary, and init the el-get package given by
PACKAGE, a symbol"
  (let ((p (symbol-name package)))
    (if (string= (el-get-package-status p) "installed")
        (el-get-init p)
      (el-get-do-install p))))

(defun el-get-dependency-installed (package dependency)
  "Install the given PACKAGE (a symbol) iff all its dependencies
are now installed"
  (when (every 'el-get-package-initialized-p
               (el-get-dependencies package))
    (el-get-demand1 package)))

(defun el-get-dependency-error (package dependency data)
  "Mark PACKAGE as having failed installation due to a failure to
  install DEPENDENCY, with error information DATA"
  (el-get-mark-failed package (list dependency data)))


(defun el-get-install (package)
  "Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol."
  (interactive (list (el-get-read-package-name "Install")))
  (if (el-get-package-is-installed package)
      (message "el-get: `%s' package is already installed" package)

    (condition-case err
	(let* ((psym (el-get-as-symbol package))
	       (pname (symbol-name psym)))

	  ;; don't do anything if it's already installed or in progress
	  (unless (memq (el-get-package-state psym) '(init installing))

	    ;; Remember that we're working on it
	    (el-get-set-package-state psym 'installing)

	    (let ((non-installed-dependencies
		   (remove-if 'el-get-package-initialized-p
			      (el-get-dependencies psym))))

	      ;;
	      ;; demand all non-installed dependencies with appropriate
	      ;; handlers in place to trigger installation of this package
	      ;;
	      (dolist (dep non-installed-dependencies)
		;; set up a handler that will install `package' when all
		;; its dependencies are installed
		(el-get-add-generic-event-task
		 (el-get-event-id dep 'init)
		 `(lambda (data)
		    (el-get-mark-initialized ',dep)
		    (el-get-dependency-installed ',psym ',dep)))

		;; set up a handler that will cancel installation of
		;; `package' if installing the dependency fails
		(el-get-add-generic-event-task
		 (el-get-event-id dep 'error)
		 `(lambda (data)
		    (el-get-set-package-state ',dep (list 'error data))
		    (el-get-dependency-error ',psym ',dep data)))

		(el-get-install dep))

	      (unless non-installed-dependencies
		(el-get-demand1 psym)))))
      ((debug error)
       (el-get-installation-failed package err)))))

(defun el-get-installation-failed (package signal-data)
  "Run all the failure hooks for PACKAGE and `signal' the car and cdr of SIGNAL-DATA."
  (run-hook-with-args 'el-get-post-error-hooks package signal-data)
  (signal (car signal-data) (cdr signal-data)))


;;
;; call-process-list utility
;;
(defun el-get-start-process-list-sentinel (proc change)
  "When proc has exited and was successful, chain next command."
  (when (eq (process-status proc) 'exit)
    (condition-case err
        (let ((status  (process-exit-status proc))
              (cname   (process-get proc :command-name))
              (cbuf    (process-get proc :buffer-name))
              (message (process-get proc :message))
              (errorm  (process-get proc :error))
              (package (process-get proc :el-get-package))
              (final-f (process-get proc :el-get-final-func))
              (next    (process-get proc :el-get-start-process-list))
              (el-get-sources (process-get proc :el-get-sources)))
          (if (not (eq 0 status))
              (progn
                (when (process-buffer proc)
                  (set-window-buffer (selected-window) cbuf))
                (error "el-get: %s %s" cname errorm))
            (message "el-get: %s" message))

          (when cbuf (kill-buffer cbuf))
          (if next
              (el-get-start-process-list package next final-f)
            (when (functionp final-f)
              (funcall final-f package))))
      ((debug error)
       (el-get-installation-failed (process-get proc :el-get-package) err)))))

(defvar el-get-default-process-sync nil
  "Non-nil value asks `el-get-start-process-list' to run current
process synchronously. Can be overridden by :sync property in
commands argument of `el-get-start-process-list'")

(defun el-get-start-process-list (package commands final-func)
  "Run each command one after the other, in order, stopping at
first error.

Commands should be a list of plists with at least the following
properties:

:default-directory

   default-directory from where to start the command

:command-name

   Name of the command to start, gives the name of the Emacs subprocess.

:buffer-name

   Name of the buffer associated with the command.

:process-filter

   Function to use as a process filter.

:shell

   When set to a non-nil value, use start-process-shell-command
   rather than the default start-process.

:program

   The program to start

:args

   The list of arguments for the program to start

:message

   The message to send upon success

:error

   The error to send upon failure

:sync

   When set to non-nil value, run synchronously.

:stdin

   Standard input to use for the process.  A lisp value is
   expected, it will get `prin1-to-string' then either saved to a
   file for a synchronous process or sent with
   `process-send-string' for an asynchronous one.

Any other property will get put into the process object.
"
  (condition-case err
      (if commands
        (let* ((c       (car commands))
               (cdir    (plist-get c :default-directory))
               (cname   (plist-get c :command-name))
               (cbuf    (plist-get c :buffer-name))
               (killed  (when (get-buffer cbuf) (kill-buffer cbuf)))
               (filter  (plist-get c :process-filter))
               (program (plist-get c :program))
               (args    (plist-get c :args))
               (shell   (plist-get c :shell))
               (sync    (if (plist-member c :sync) (plist-get c :sync)
                          el-get-default-process-sync))
	       (stdin   (plist-get c :stdin))
               (default-directory (if cdir
                                      (file-name-as-directory
                                       (expand-file-name cdir))
                                    default-directory)))
          (if sync
              (let* ((startf (if shell #'call-process-shell-command #'call-process))
		     (infile (when stdin (make-temp-file "el-get")))
		     (dummy  (when infile
			       (with-temp-file infile
				 (insert (prin1-to-string stdin)))))
                     (dummy  (message "el-get is waiting for %S to complete" cname))
		     (status (apply startf program infile cbuf t args))
                     (message (plist-get c :message))
                     (errorm  (plist-get c :error))
                     (next    (cdr commands)))
		(when el-get-verbose
		  (message "%S" (with-current-buffer cbuf (buffer-string))))
                (if (eq 0 status)
                    (message "el-get: %s" message)
                  (set-window-buffer (selected-window) cbuf)
                  (error "el-get: %s %s" cname errorm))
                (when cbuf (kill-buffer cbuf))
                (if next
                    (el-get-start-process-list package next final-func)
                  (when (functionp final-func)
                    (funcall final-func package))))
            ;; async case
            (let* ((startf (if shell #'start-process-shell-command #'start-process))
                   (process-connection-type nil) ; pipe, don't pretend we're a pty
                   (proc (apply startf cname cbuf program args)))
              ;; add the properties to the process, then set the sentinel
              (mapc (lambda (x) (process-put proc x (plist-get c x))) c)
              (process-put proc :el-get-sources el-get-sources)
              (process-put proc :el-get-package package)
              (process-put proc :el-get-final-func final-func)
              (process-put proc :el-get-start-process-list (cdr commands))
	      (when stdin
		(process-send-string proc (prin1-to-string stdin))
		(process-send-eof proc))
              (set-process-sentinel proc 'el-get-start-process-list-sentinel)
              (when filter (set-process-filter proc filter)))))
	;; no commands, still run the final-func
	(when (functionp final-func)
	  (funcall final-func package)))
    ((debug error)
     (el-get-installation-failed package err))))

;;
;; get an executable given its command name, with friendly error message
;;
(defun el-get-executable-find (name)
  "Return the absolute path of the command to execute, and errors
out if that can not be found.

This function will first look for existing function named
\"el-get-NAME-executable\" and call that. This function, if it
exists, must handle error cases.

Then, it will look for existing variable named \"el-get-NAME\"
and error if that's not nil and not an existing file name.

Baring variable named \"el-get-NAME\", it will call
`executable-find' on NAME and use the output of that, or error
out if it's nil."
  (let ((fname (intern (format "el-get-%s-executable" name)))
	(vname (intern (format "el-get-%s" name))))
    (cond
     ((fboundp fname)
      (funcall fname))

     ;; vname is bound here, we want to check for the variable named vname
     ;; (bound-and-true-p vname) won't cut it
     ((ignore-errors (symbol-value vname))
      (let ((command (symbol-value vname)))
	(unless (and (file-exists-p command)
		     (file-executable-p command))
	  (error
	   (concat "The variable `%s' points to \"%s\", "
		   "which is not an executable file name on your system.")
	   name command))
	command))

     (t
      (let ((command (executable-find name)))
	(unless command
	  (error
	   "The command named '%s' can not be found with `executable-find'"
	   name))
	command)))))


;;
;; git support
;;
(defun el-get-git-executable ()
  "Return git executable to use, or signal an error when not
found."
  (let ((git-executable (if (and (boundp 'magit-git-executable)
				 (file-executable-p magit-git-executable))
			    magit-git-executable
			  (executable-find "git"))))
    (unless (and git-executable (file-executable-p git-executable))
      (error
       (concat "el-get-git-clone requires `magit-git-executable' to be set, "
	       "or the binary `git' to be found in your PATH")))
    git-executable))

(defun el-get-git-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir   (el-get-package-directory package))
	 (name   (format "*git clone %s*" package))
	 (source (el-get-package-def package))
	 (branch (plist-get source :branch))
	 (args   (if branch
		     (list "--no-pager" "clone" "-b" branch url package)
		   (list "--no-pager" "clone" url package)))
	 (ok     (format "Package %s installed." package))
	 (ko     (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ,args
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir (el-get-package-directory package))
	 (name (format "*git pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ( "--no-pager" "pull")
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-update-fun)))

;;
;; emacsmirror support
;;
(defun el-get-emacsmirror-clone (package url post-install-fun)
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
	 (url     (or url (format el-get-emacsmirror-base-url pkgname))))
    (el-get-git-clone package url post-install-fun)))


;;
;; git-svn support
;;
(defun el-get-git-svn-clone (package url post-install-fun)
  "Clone the given svn PACKAGE following the URL using git."
  (let ((git-executable (el-get-executable-find "git"))
	(name (format "*git svn clone %s*" package))
	(ok   (format "Package %s installed." package))
	(ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ( "--no-pager" "svn" "clone" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-git-svn-update (package url post-update-fun)
  "Update PACKAGE using git-svn. URL is given for compatibility reasons."
  (let ((git-executable (el-get-executable-find "git"))
	(pdir   (el-get-package-directory package))
	(f-name (format "*git svn fetch %s*" package))
	(f-ok   (format "Fetched package %s." package))
	(f-ko   (format "Could not fetch package %s." package))
	(r-name (format "*git svn rebase %s*" package))
	(r-ok   (format "Rebased package %s." package))
	(r-ko   (format "Could not rebase package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,f-name
		      :buffer-name ,f-name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "svn" "fetch")
		      :message ,f-ok
		      :error ,f-ko)

       (:command-name ,r-name
		      :buffer-name ,r-name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "svn" "rebase")
		      :message ,r-ok
		      :error ,r-ko))
     post-update-fun)))


;;
;; bzr support
;;
(defun el-get-bzr-branch (package url post-install-fun)
  "Branch a given bzr PACKAGE following the URL using bzr."
  (let* ((bzr-executable (el-get-executable-find "bzr"))
	 (name (format "*bzr branch %s*" package))
	 (ok   (format "Package %s installed" package))
	 (ko   (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,bzr-executable
		      :args ("branch" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-bzr-pull (package url post-update-fun)
  "bzr pull the package."
  (let* ((bzr-executable (el-get-executable-find "bzr"))
	 (pdir (el-get-package-directory package))
	 (name (format "*bzr pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,bzr-executable
		      :args ( "pull" )
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; svn support
;;
(defun el-get-svn-checkout (package url post-install-fun)
  "svn checkout the package."
  (let* ((svn-executable (el-get-executable-find "svn"))
	 (source  (el-get-package-def package))
	 (name    (format "*svn checkout %s*" package))
	 (ok      (format "Checked out package %s." package))
	 (ko      (format "Could not checkout package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,svn-executable
		      :args ("checkout" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-svn-update (package url post-update-fun)
  "update the package using svn."
  (let* ((svn-executable (el-get-executable-find "svn"))
	 (pdir (el-get-package-directory package))
	 (name (format "*svn update %s*" package))
	 (ok   (format "Updated package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,svn-executable
		      :args ("update")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; CVS support
;;
(defun el-get-cvs-checkout (package url post-install-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
	 (source  (el-get-package-def package))
	 (module  (plist-get source :module))
	 (options (plist-get source :options))
	 (name    (format "*cvs checkout %s*" package))
	 (ok      (format "Checked out package %s." package))
	 (ko      (format "Could not checkout package %s." package)))

    ;; (message "%S" `(:args ("-d" ,url "checkout" "-d" ,package ,module)))
    ;; (message "el-get-cvs-checkout: %S" (string= options "login"))

    (el-get-start-process-list
     package
     `(,@(when (string= options "login")
	   `((:command-name ,(format "*cvs login %s*" package)
			    :buffer-name ,(format "*cvs login %s*" package)
			    :default-directory ,el-get-dir
			    :process-filter ,(function el-get-sudo-password-process-filter)
			    :program ,cvs-executable
			    :args ("-d" ,url "login")
			    :message "cvs login"
			    :error "Could not login against the cvs server")))

       (:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,cvs-executable
		      :args ("-d" ,url "checkout" "-d" ,package ,module)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-cvs-update (package url post-update-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
	 (pdir (el-get-package-directory package))
	 (name (format "*cvs update %s*" package))
	 (ok   (format "Updated package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,cvs-executable
		      :args ("update" "-dP")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; darcs support
;;
(defun el-get-darcs-get (package url post-install-fun)
  "Get a given PACKAGE following the URL using darcs."
  (let* ((darcs-executable (el-get-executable-find "darcs"))
	 (name (format "*darcs get %s*" package))
	 (ok   (format "Package %s installed" package))
	 (ko   (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,darcs-executable
		      :args ("get" "--lazy" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-darcs-pull (package url post-update-fun)
  "darcs pull the package."
  (let* ((darcs-executable (el-get-executable-find "darcs"))
	 (pdir (el-get-package-directory package))
	 (name (format "*darcs pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,darcs-executable
		      :args ( "pull" "--all")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; utilities for both apt-get and fink support (dpkg based)
;;
(defun el-get-dpkg-package-status (package)
  "Return the package status from dpkg --get-selections."
  (substring
   (shell-command-to-string
    (format
     "dpkg -l %s| awk '/^ii/ && $2 = \"%s\" {print \"ok\"}'" package package)) 0 -1))

;;
;; those functions are meant as hooks at install and remove, and they will
;; get the global value of package, which has been set before calling
;; run-hooks.
;;
(defun el-get-dpkg-symlink (package)
  "ln -s /usr/share/emacs/site-lisp/package ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package))
	 (method  (el-get-package-method package))
	 (basedir (cond ((eq method 'apt-get) el-get-apt-get-base)
			((eq method 'fink)    el-get-fink-base)
			((eq method 'pacman)  el-get-pacman-base)))
	 (debdir  (concat (file-name-as-directory basedir) package)))
    (unless (file-directory-p pdir)
      (shell-command
       (concat "cd " el-get-dir " && ln -s " debdir  " " package)))))

(defun el-get-dpkg-remove-symlink (package)
  "rm -f ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package)))
    (when (file-symlink-p pdir)
      (let ((command (concat "cd " el-get-dir " && rm -f " package)))
        (message command)
        (shell-command command)))))


;;
;; apt-get support
;;
(add-hook 'el-get-apt-get-install-hook 'el-get-dpkg-symlink)

(defvar el-get-sudo-password-process-filter-pos)

(defun el-get-sudo-password-process-filter (proc string)
  "Filter function that fills the process buffer's and matches a
password prompt."
  (unless (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      ;; arrange to remember already seen content
      (unless (boundp 'el-get-sudo-password-process-filter-pos)
	(make-local-variable 'el-get-sudo-password-process-filter-pos)
	(setq el-get-sudo-password-process-filter-pos (point-min)))

      ;; first, check about passwords
      (save-excursion
	(goto-char (point-max))
	(insert string)
	;; redirect the subprocess sudo prompt to the user face, and answer it
	(goto-char el-get-sudo-password-process-filter-pos)
	(while (re-search-forward "password" nil t)
	  (let* ((prompt (thing-at-point 'line))
		 (pass   (read-passwd prompt)))
	    (process-send-string proc (concat pass "\n")))))

      ;; second, check about "Do you want to continue [Y/n]?" prompts
      (save-excursion
	(while (re-search-forward "Do you want to continue" nil t)
	  (set-window-buffer (selected-window) (process-buffer proc))
	  (let* ((prompt (thing-at-point 'line))
		 (cont   (yes-or-no-p (concat prompt " "))))
	    (process-send-string proc (concat (if cont "y" "n") "\n")))))

      (setq el-get-sudo-password-process-filter-pos (point-max)))))

(defun el-get-apt-get-install (package url post-install-fun)
  "echo $pass | sudo -S apt-get install PACKAGE"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
         (name (format "*apt-get install %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "apt-get") "install" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-apt-get-remove (package url post-remove-fun)
  "apt-get remove PACKAGE, URL is there for API compliance"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
         (name (format "*apt-get remove %s*" package))
	 (ok   (format "Package %s removed." package))
	 (ko   (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "apt-get") "remove" "-y" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-apt-get-remove-hook 'el-get-dpkg-remove-symlink)


;;
;; fink support
;;
(defun el-get-fink-install (package url post-install-fun)
  "sudo -S fink install PACKAGE"
  (let* ((name (format "*fink install %s*" package))
	 (source  (el-get-package-def package))
	 (pkgname (or (plist-get source :pkgname) package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "fink") "install" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(add-hook 'el-get-fink-install-hook 'el-get-dpkg-symlink)

(defun el-get-fink-remove (package url post-remove-fun)
  "apt-get remove PACKAGE. URL is there for API compliance."
  (let* ((name (format "*fink remove %s*" package))
	 (source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
	 (ok   (format "Package %s removed." package))
	 (ko   (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "fink") "-y" "remove" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-fink-remove-hook 'el-get-dpkg-remove-symlink)


;;
;; ELPA support
;;
(defun el-get-elpa-package-directory (package)
  "Return the directory where ELPA stores PACKAGE, or nil if
PACKAGE isn't currently installed by ELPA."
  (let* ((pname (format "%s" package))  ; easy way to cope with symbols etc.

	 (l
	  ;; we use try-completion to find the realname of the directory
	  ;; ELPA used, and this wants an alist, we trick ls -i -1 into
	  ;; that.
	  (mapcar 'split-string
		  (split-string
		   (shell-command-to-string
		    (concat
		     "ls -i1 "
		     (expand-file-name
		      (file-name-as-directory package-user-dir)))))))

	 (realname (try-completion pname l)))

    (if realname (concat (file-name-as-directory package-user-dir) realname)
      realname)))

(defun el-get-elpa-package-repo (package)
  "Get the ELPA repository cons cell for PACKAGE.

The cons cell has the form (NAME . URL). See `package-archives'.
If the package source only specifies a URL, the URL will be used
for NAME as well.

If PACKAGE's `:type' is not \"elpa\", or no repo is specified in
the recipe, then return nil."
  (let* ((source (el-get-package-def package))
         (type   (el-get-package-type source))
         (elpa-repo (plist-get source :repo)))
    (when (and (eq type 'elpa) elpa-repo)
      (cond ((stringp elpa-repo)
             (cons elpa-repo elpa-repo))
            ((consp elpa-repo)
             (if (and (stringp (car elpa-repo))
                      (stringp (cdr elpa-repo)))
                 elpa-repo
               (error "Invalid elpa repo spec: %s" elpa-repo)))
            (t
             (error "Invalid elpa repo spec: %s" elpa-repo))))))

(defun el-get-elpa-symlink-package (package)
  "ln -s ../elpa/<package> ~/.emacs.d/el-get/<package>"
  (let ((elpa-dir (file-relative-name
		   (el-get-elpa-package-directory package) el-get-dir)))
    (unless (el-get-package-exists-p package)
      ;; better style would be to check for (fboundp 'make-symbolic-link) but
      ;; that would be true on Vista, where by default only administrator is
      ;; granted to use the feature --- so hardcode those systems out
      (if (memq system-type '(ms-dos windows-nt))
	  ;; the symlink is a docs/debug feature, mkdir is ok enough
	  (make-directory (el-get-package-directory package))
	(message "%s"
		 (shell-command
		  (concat "cd " el-get-dir
			  " && ln -s \"" elpa-dir "\" \"" package "\"")))))))

(defun el-get-elpa-install (package url post-install-fun)
  "Ask elpa to install given PACKAGE."
  (let* ((elpa-dir (el-get-elpa-package-directory package))
         (elpa-repo (el-get-elpa-package-repo package))
         ;; Set `package-archive-base' to elpa-repo for old package.el
         (package-archive-base (or (cdr-safe elpa-repo)
                                   (bound-and-true-p package-archive-base)))
         ;; Prepend elpa-repo to `package-archives' for new package.el
         (package-archives (append (when elpa-repo (list elpa-repo))
                                   (when (boundp 'package-archives) package-archives))))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      ;; Make sure we have got *some* kind of record of the package archive.
      ;; TODO: should we refresh and retry once if package-install fails?
      (let ((p (if (fboundp 'package-read-all-archive-contents)
		   (package-read-all-archive-contents) ; version from emacs24
		 (package-read-archive-contents)))     ; old version
            ;; package-install generates autoloads, byte compiles
            emacs-lisp-mode-hook fundamental-mode-hook prog-mode-hook)
	(unless p
	  (package-refresh-contents)))
      (package-install (intern package)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package))
  (funcall post-install-fun package))

(defun el-get-elpa-update (package url post-update-fun)
  "Ask elpa to update given PACKAGE."
  (el-get-elpa-remove package url nil)
  (package-refresh-contents)
  (package-install (intern package))
  (funcall post-update-fun package))

(defun el-get-elpa-remove (package url post-remove-fun)
  "Remove the right directory where ELPA did install the package."
  (el-get-rmdir package url post-remove-fun))

(defun el-get-elpa-post-remove (package)
  "Do remove the ELPA bits for package, now"
  (let ((p-elpa-dir (el-get-elpa-package-directory package)))
    (if p-elpa-dir
	(dired-delete-file p-elpa-dir 'always)
      (message "el-get: could not find ELPA dir for %s." package))))

(add-hook 'el-get-elpa-remove-hook 'el-get-elpa-post-remove)


;;
;; http support
;;
(defun el-get-filename-from-url (url)
  "return a suitable filename from given url

Test url: http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el"
  (replace-regexp-in-string "[^a-zA-Z0-9-_\.\+]" "_"
			    (file-name-nondirectory url)))

(defun el-get-http-retrieve-callback (status package post-install-fun &optional dest sources)
  "Callback function for `url-retrieve', store the emacs lisp file for the package."
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (or dest (concat (file-name-as-directory pdir) package ".el")))
	 (part   (concat dest ".part"))
	 (el-get-sources (if sources sources el-get-sources))
	 (buffer-file-coding-system 'no-conversion)
	 (require-final-newline nil))
    ;; prune HTTP headers before save
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (write-file part)
    (when (file-exists-p dest)
      (delete-file dest))
    (rename-file part dest)
    (message "Wrote %s" dest)
    (kill-buffer))
  (funcall post-install-fun package))

(defun el-get-http-install (package url post-install-fun &optional dest)
  "Dowload a single-file PACKAGE over HTTP and store it in DEST.

Should dest be omitted (nil), the url content will get written
into the package :localname option or its `file-name-nondirectory' part."
  (let* ((pdir   (el-get-package-directory package))
	 (fname  (or (plist-get (el-get-package-def package) :localname)
		     (el-get-filename-from-url url)))
	 (dest   (or dest
		     (concat (file-name-as-directory pdir) fname))))
    (unless (file-directory-p pdir)
      (make-directory pdir))

    (if (not el-get-default-process-sync)
        (url-retrieve url 'el-get-http-retrieve-callback
                      `(,package ,post-install-fun ,dest ,el-get-sources))

      (with-current-buffer (url-retrieve-synchronously url)
        (el-get-http-retrieve-callback
	 nil package post-install-fun dest el-get-sources)))))



;;
;; EmacsWiki support, which is http but with a known URL
;;
(defun el-get-emacswiki-install (package url post-install-fun)
  "Download a single-file PACKAGE over HTTP from emacswiki."
  (let ((url (or url (format el-get-emacswiki-base-url package))))
    (el-get-http-install package url post-install-fun)))

(defun el-get-emacswiki-retrieve-package-list ()
  "returns a list of (URL . PACKAGE) from emacswiki listing page"
  (with-current-buffer
      (url-retrieve-synchronously el-get-emacswiki-elisp-index-url)
    (goto-char (point-min))
    (re-search-forward "pages found.</h2>" nil 'move)
    (remove-if-not
     (lambda (p) (string-match "el$" (cdr p)))
     (loop
      with offset = (length el-get-emacswiki-elisp-index-base-url)
      ;; <a class="local" href="http://www.emacswiki.org/emacs/thingatpt%2b.el">thingatpt+.el</a>
      while (re-search-forward el-get-emacswiki-elisp-index-base-url nil 'move)
      collect (cons
	       ;; URL
	       (buffer-substring-no-properties
		(- (point) offset)
		(1- (re-search-forward "\"" nil 'move)))
	       ;; PACKAGE name
	       (buffer-substring-no-properties
		(re-search-forward ">" nil 'move)
		(1- (re-search-forward "<" nil 'move))))))))

(defun el-get-emacswiki-build-local-recipes (&optional target-dir)
  "retrieve the index of elisp pages at emacswiki and turn them
into a local recipe file set"
  (let ((target-dir (or target-dir
			(car command-line-args-left)
			el-get-recipe-path-emacswiki)))
    (unless (file-directory-p target-dir) (make-directory target-dir))
    (loop
     for (url . package) in (el-get-emacswiki-retrieve-package-list)
     for recipe = (replace-regexp-in-string "el$" "rcp" package)
     for rfile  = (expand-file-name recipe target-dir)
     unless (file-exists-p rfile)
     do (with-temp-file (expand-file-name rfile target-dir)
	  (message "%s" package)
	  (insert (format "(:name %s :type emacswiki :website \"%s\")"
			  (file-name-sans-extension package) url))))))

(defun el-get-emacswiki-refresh (&optional target-dir)
  "run Emacs -Q in an asynchronous subprocess to get the package
list from emacswiki and build a local recipe directory out of
that"
  (interactive
   (list (let ((dummy (unless (file-directory-p el-get-recipe-path-emacswiki)
			(make-directory el-get-recipe-path-emacswiki))))
	   (read-directory-name "emacswiki recipes go to: "
				el-get-recipe-path-emacswiki))))
  (let* ((name "*el-get-emacswiki*")
	 (dummy (when (get-buffer name) (kill-buffer name)))
	 (args
	  (format "-Q -batch -l %s -f el-get-emacswiki-build-local-recipes %s"
		  (file-name-sans-extension
		   (symbol-file 'el-get-emacswiki-build-local-recipes 'defun))
		  target-dir))
	 (process
	  (apply 'start-process name name el-get-emacs (split-string args))))
    (message "%s %s" el-get-emacs args)
    (set-process-sentinel
     process
     '(lambda (proc event)
	(when (eq (process-status proc) 'exit)
	  (el-get-notify "el-get: EmacsWiki"
			 "EmacsWiki local recipe list refreshed"))))))


;;
;; http-tar support (archive)
;;
(defun el-get-http-tar-cleanup-extract-hook (package)
  "Cleanup after tar xzf: if there's only one subdir, move all
the files up."
  (let* ((pdir    (el-get-package-directory package))
	 (url     (plist-get (el-get-package-def package) :url))
	 (tarfile (el-get-filename-from-url url))
	 (files   (remove tarfile (directory-files pdir nil "[^.]$")))
	 (dir     (car files)))
    ;; if there's only one directory, move its content up and get rid of it
    (el-get-verbose-message "el-get: tar cleanup %s [%s]: %S" package pdir files)
    (unless (cdr files)
      (loop for fname in (directory-files
			  (expand-file-name dir pdir) nil "[^.]$")
	    for fullname = (expand-file-name fname (expand-file-name dir pdir))
	    for newname  = (expand-file-name pdir fname)
	    do (progn
		 (el-get-verbose-message "%S %S %S" pdir dir fname)
		 (el-get-verbose-message "mv %S %S" fullname newname)
		 (rename-file fullname newname)))
      (el-get-verbose-message "delete-directory: %s" (expand-file-name dir pdir))
      (delete-directory (expand-file-name dir pdir)))))

(defun el-get-http-tar-install (package url post-install-fun)
  "Dowload a tar archive package over HTTP."
  (let* ((source  (el-get-package-def package))
	 (options (plist-get source :options))
	 (pdir    (el-get-package-directory package))
	 (tarfile (el-get-filename-from-url url))
	 (dest    (concat (file-name-as-directory pdir) tarfile))
	 (name    (format "*tar %s %s*" options url))
	 (ok      (format "Package %s installed." package))
	 (ko      (format "Could not install package %s." package))
	 (post `(lambda (package)
		  ;; tar xzf `basename url`
		  (let ((el-get-sources '(,@el-get-sources)))
		    (el-get-start-process-list
		     package
		     '((:command-name ,name
				      :buffer-name ,name
				      :default-directory ,pdir
				      :program ,(executable-find "tar")
				      :args (,@options ,tarfile)
				      :message ,ok
				      :error ,ko))
		     ,(symbol-function post-install-fun))))))
    (el-get-http-install package url post dest)))

(add-hook 'el-get-http-tar-install-hook 'el-get-http-tar-cleanup-extract-hook)


;;
;; pacman support
;;
(add-hook 'el-get-pacman-install-hook 'el-get-dpkg-symlink)

(defun el-get-pacman-install (package url post-install-fun)
  "echo $pass | sudo -S pacman install PACKAGE"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
         (name    (format "*pacman install %s*" package))
	 (ok      (format "Package %s installed." package))
	 (ko      (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "pacman") "--sync" "--noconfirm" "--needed" ,pkgname)
		      :message ,ok
		      :error ,ko
		      :sync t))
     post-install-fun)))

(defun el-get-pacman-remove (package url post-remove-fun)
  "pacman remove PACKAGE, URL is there for API compliance"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) package))
         (name    (format "*pacman remove %s*" package))
	 (ok      (format "Package %s removed." package))
	 (ko      (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "pacman") "--remove" "--noconfirm" ,pkgname)
		      :message ,ok
		      :error ,ko
		      :sync t))
     post-remove-fun)))

(add-hook 'el-get-pacman-remove-hook 'el-get-dpkg-remove-symlink)


;;
;; mercurial (hg) support
;;
(defun el-get-hg-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((hg-executable (el-get-executable-find "hg"))
	 (pdir (el-get-package-directory package))
	 (name (format "*hg clone %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,hg-executable
		      :args ("clone" ,url ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-hg-pull (package url post-update-fun)
  "hg pull the package."
  (let* ((hg-executable (el-get-executable-find "hg"))
	 (pdir (el-get-package-directory package))
	 (name (format "*hg pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,hg-executable
		      :args ("pull" "--update")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))


;;
;; Common support bits
;;
(defun el-get-rmdir (package url post-remove-fun)
  "Just rm -rf the package directory. Follow symlinks."
  (let* ((source   (el-get-package-def package))
	 (method   (el-get-package-method source))
	 (pdir (el-get-package-directory package)))
    (if (eq method 'elpa)
	;; only remove a symlink here
	(when (or (file-symlink-p (directory-file-name pdir))
                  (file-exists-p pdir))
	  (delete-file (directory-file-name pdir)))
      ;; non ELPA packages, remove the directory
      (if (file-exists-p pdir)
	  (dired-delete-file pdir 'always)
	(message "el-get could not find package directory \"%s\"" pdir))
      (funcall post-remove-fun package))))

(defun el-get-set-info-path (package infodir-rel)
  (require 'info)
  (info-initialize)
  (el-get-add-path-to-list package 'Info-directory-list infodir-rel))

(defun el-get-install-or-init-info (package build-or-init)
  "Call `el-get-install-info' to create the necessary \"dir\"
  file when build-or-init is 'build, or `el-get-set-info-path'
  when build-or-init is 'init "
  (let* ((source   (el-get-package-def package))
	 (method   (el-get-package-method source))
	 (infodir  (plist-get source :info))
	 (pdir     (el-get-package-directory package)))

    ;; apt-get, pacman and ELPA will set up Info-directory-list
    (unless (member method '(elpa apt-get fink pacman))
      (let* ((infodir-abs-conf (concat pdir infodir))
	     (infodir-abs (file-name-as-directory
                           (if (file-directory-p infodir-abs-conf)
                               infodir-abs-conf
                             (file-name-directory infodir-abs-conf))))
	     (infodir-rel (if (file-directory-p infodir-abs-conf)
			      infodir
			    (file-name-directory infodir)))
	     (info-dir    (concat infodir-abs "dir"))
	     (infofile (if (and (file-exists-p infodir-abs-conf)
				(not (file-directory-p infodir-abs-conf)))
			   infodir-abs-conf
			 (concat infodir-abs package))))

	(cond
	 ((eq build-or-init 'init)
	  (when (file-exists-p info-dir)
	    (el-get-set-info-path package infodir-rel)))

	  ((eq build-or-init 'build)
	   ;; rebuild each time asked --- e.g. on update
	   (when (and infodir
		      (file-directory-p infodir-abs)
		      (not (file-exists-p info-dir)))
	     (el-get-set-info-path package infodir-rel)
	     (el-get-build
	      package
	      `(,(format "%s %s dir"
			 el-get-install-info
			 (if (string= (substring infofile -5) ".info")
			     infofile
			   (concat infofile ".info")))) infodir-rel t nil t)))

	  (t
	   (error
	    "el-get-install-or-init-info: %s not supported" build-or-init)))))))

;; Emacs < 24
(eval-and-compile
  (if (fboundp 'byte-recompile-file)
      (defsubst el-get-byte-compile-file (el)
        ;; Byte-compile runs emacs-lisp-mode-hook; disable it
        (let (emacs-lisp-mode-hook)
          (byte-recompile-file el)))
    (defun el-get-byte-compile-file (el)
      "Same as `byte-compile-file', but skips unnecessary compilation.

Specifically, if the compiled elc file already exists and is
newer, then compilation will be skipped."
      (let ((elc (concat (file-name-sans-extension el) ".elc"))
            ;; Byte-compile runs emacs-lisp-mode-hook; disable it
            emacs-lisp-mode-hook)
        (when (or (not (file-exists-p elc))
                  (file-newer-than-file-p el elc))
          (condition-case err
              (byte-compile-file el)
            ((debug error) ;; catch-all, allow for debugging
             (message "%S" (error-message-string err)))))))))

(defun el-get-byte-compile-file-or-directory (file)
  "Byte-compile FILE or all files within it if it is a directory."
  (let ((byte-compile-warnings nil)
        ;; Byte-compile runs emacs-lisp-mode-hook; disable it
        emacs-lisp-mode-hook)
    (if (file-directory-p file)
        (byte-recompile-directory file 0)
      (el-get-byte-compile-file file))))

(defun el-get-assemble-files-for-byte-compilation (package)
  "Assemble a list of *absolute* paths to byte-compile for PACKAGE."
  (when el-get-byte-compile
    (let* ((source   (el-get-package-def package))
           (comp-prop (plist-get source :compile))
           (compile (el-get-as-list comp-prop))
           ;; nocomp is true only if :compile is explicitly set to nil.
           (explicit-nocomp (and (plist-member source :compile)
                                 (not comp-prop)))
	   (method   (el-get-package-method source))
	   (pdir     (el-get-package-directory package))
	   (el-path  (el-get-load-path package))
	   (files '()))
      (cond
       (compile
        ;; only byte-compile what's in the :compile property of the recipe
        (dolist (path compile)
          (let ((fullpath (expand-file-name path pdir)))
            (if (file-exists-p fullpath)
                ;; path is a file/dir, so add it literally
                (add-to-list 'files fullpath)
              ;; path is a regexp, so add matching file names in package dir
              (mapc (apply-partially 'add-to-list 'files)
		    (directory-files pdir nil fullpath))))))

       ;; If package has (:compile nil), or package has its own build
       ;; instructions, or package is already pre-compiled by the
       ;; installation method, then don't compile anything.
       ((or explicit-nocomp
            (el-get-build-commands package)
            (member method '(apt-get fink pacman)))
        nil)

       ;; Default: compile the package's entire load-path
       (t
        (mapc (apply-partially 'add-to-list 'files) el-path)))
      files)))

(defun el-get-byte-compile-from-stdin ()
  "byte compile files read on STDIN

This is run as a subprocess with an `emacs -Q -batch -f
el-get-byte-compile` command and with the file list as stdin,
written by `prin1-to-string' so that `read' is able to process
it."
  (let ((files (read)))
    (loop for f in files
	  do (progn
	       (message "el-get-byte-compile-from-stdin: %s" f)
	       (el-get-byte-compile-file-or-directory f)))))

(defun el-get-byte-compile-process (package buffer working-dir sync files)
  "return the 'el-get-start-process-list' entry to byte compile PACKAGE"
  (let ((bytecomp-command
	 (list el-get-emacs
	       "-Q" "-batch" "-f" "toggle-debug-on-error"
	       "-l" (shell-quote-argument
		     (file-name-sans-extension
		      (symbol-file 'el-get-byte-compile-from-stdin 'defun)))
	       "-f" "el-get-byte-compile-from-stdin")))
    `(:command-name "byte-compile"
		    :buffer-name ,buffer
		    :default-directory ,working-dir
		    :shell t
		    :sync ,sync
		    :stdin ,files
		    :program ,(car bytecomp-command)
		    :args ,(cdr bytecomp-command)
		    :message ,(format "el-get-build %s: byte-compile ok." package)
		    :error ,(format
			     "el-get could not byte-compile %s" package))))

(defun el-get-byte-compile (package)
  "byte compile files for given package"
  (let ((pdir  (el-get-package-directory package))
	(buf   "*el-get-byte-compile*")
	(files (el-get-assemble-files-for-byte-compilation package)))
    (when files
      (el-get-start-process-list
       package
       (list (el-get-byte-compile-process package buf pdir t files))
       nil))))

(defun el-get-build-commands (package)
  "Return a list of build commands for the named PACKAGE.

The result will either be nil; a list of strings, each one to be
interpreted as a shell command; or a list of lists of
strings, each string representing a single shell argument."
  (let* ((source     (el-get-package-def package))
         (build-type (intern (format ":build/%s" system-type)))
         (build-commands
	   (or (plist-get source build-type)
	       (plist-get source :build))))

    (unless (listp build-commands)
      (error "build commands for package %s are not a list" package))

    (unless (stringp (car build-commands))
      (setq build-commands (eval build-commands)))

    (mapcar (lambda (x) (if (stringp x) x (el-get-flatten x)))
            build-commands)))

(defun el-get-build-command-program (name)
  "Given the user command name, get the command program to execute.

That will find the program in current $PATH for you, unless given
command name is a relative filename beginning with \"./\", or its
absolute filename obtained with expand-file-name is executable."
  (let ((fullname (expand-file-name name))
	(exe      (executable-find name)))
    (cond ((string-match "^\./" name)   name)
	  ((file-executable-p fullname) fullname)
	  (t (or exe name)))))

(defun el-get-build
  (package commands &optional subdir sync post-build-fun installing-info)
  "Run each command from the package directory.

COMMANDS is a list of commands to run in order to build the
package.

The commands are run either synchronously or asynchronously
depending on the SYNC parameter, and can be run from SUBDIR
directory when given.  By default COMMANDS are run from the
package directory as obtained by `el-get-package-directory'.

The function POST-BUILD-FUN will get called after the commands
are all successfully run.  In case of asynchronous building, the
only way to have code running after the build is using this
parameter.

INSTALLING-INFO is t when called from
`el-get-install-or-init-info', as to avoid a nasty infinite
recursion.
"
  (el-get-verbose-message "el-get-build %s" package)
  (let* ((pdir   (el-get-package-directory package))
	 (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir))
	 (buf    (format "*el-get-build: %s*" package))
	 (default-directory (file-name-as-directory wdir))
	 (process-list
	  (mapcar (lambda (c)
		    (let* ((split    (if (stringp c)
					 (split-string c)
				       (mapcar 'shell-quote-argument c)))
			   (c        (mapconcat 'identity split " "))
			   (name     (car split))
			   (program  (el-get-build-command-program name))
			   (args     (cdr split)))

		      `(:command-name ,name
				      :buffer-name ,buf
				      :default-directory ,wdir
				      :shell t
				      :sync sync
				      :program ,program
				      :args (,@args)
				      :message ,(format "el-get-build %s: %s ok." package c)
				      :error ,(format
					       "el-get could not build %s [%s]" package c))))
		  commands))
	 (bytecomp-files (when el-get-byte-compile
			   (el-get-assemble-files-for-byte-compilation package)))
	 (full-process-list ;; includes byte compiling
	  (append
	   (when bytecomp-files
	     (list
	      (el-get-byte-compile-process package buf wdir sync bytecomp-files)))
	   process-list))
	 ;; unless installing-info, post-build-fun should take care of
	 ;; building info too
	 (build-info-then-post-build-fun
	  (if installing-info post-build-fun
	    `(lambda (package)
	       (el-get-install-or-init-info package 'build)
	       (funcall ,(if (symbolp post-build-fun)
			     (symbol-function post-build-fun)
			   ;; it must be a lambda, just inline its value
			   post-build-fun)
			package)))))

    (el-get-start-process-list
     package full-process-list build-info-then-post-build-fun)))


;;
;; recipes
;;
(defun el-get-read-recipe-file (filename)
  "Read given filename and return its content (a valid form is expected)"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (read (current-buffer))))

(defun el-get-recipe-filename (package)
  "Return the name of the file that contains the recipe for PACKAGE, if any."
  (let ((package-el  (concat (el-get-as-string package) ".el"))
	(package-rcp (concat (el-get-as-string package) ".rcp")))
    (loop for dir in el-get-recipe-path
	  for recipe-el  = (expand-file-name package-el dir)
	  for recipe-rcp = (expand-file-name package-rcp dir)
	  if (file-exists-p recipe-el)  return recipe-el
	  if (file-exists-p recipe-rcp) return recipe-rcp)))

(defun el-get-read-recipe (package)
  "Return the source definition for PACKAGE, from the recipes."
  (let ((filename (el-get-recipe-filename package)))
    (if filename
	(el-get-read-recipe-file filename)
      (error "el-get can not find a recipe for package \"%s\"." package))))

(defun el-get-read-all-recipes ()
  "Return the list of all the recipes, formatted like `el-get-sources'.

Only consider any given recipe only once even if present in
multiple dirs from `el-get-recipe-path'. The first recipe found
is the one considered.  We first look in `el-get-sources' then in
each directory listed in `el-get-recipe-path' in order."
  (let ((packages (mapcar 'el-get-source-name el-get-sources)))
    (append
     el-get-sources
     (loop for dir in (el-get-recipe-dirs)
	   nconc (loop for recipe in (directory-files dir nil "^[^.].*\.\\(rcp\\|el\\)$")
		       for filename = (concat (file-name-as-directory dir) recipe)
		       for package = (file-name-sans-extension (file-name-nondirectory recipe))
		       unless (member package packages)
		       do (push package packages)
                       and collect (ignore-errors (el-get-read-recipe-file filename)))))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for PACKAGE."
  (let ((source (loop for src in el-get-sources
		      when (string= package (el-get-source-name src))
		      return src)))

    (cond ((or (null source) (symbolp source))
	   ;; not in `el-get-sources', or only mentioned by name
	   ;; (compatibility from pre 3.1 era)
	   (el-get-read-recipe package))

	  ((null (plist-get source :type))
	   ;; we got a list with no :type, that's an override plist
	   (loop with def = (el-get-read-recipe package)
		 for (prop override) on source by 'cddr
		 do (plist-put def prop override)
		 finally return def))

	  ;; none of the previous, must be a full definition
	  (t source))))

(defun el-get-package-method (package-or-source)
  "Return the :type property (called method) of PACKAGE-OR-SOURCE"
  (cond ((or (symbolp package-or-source) (stringp package-or-source))
	 (plist-get (el-get-package-def package-or-source) :type))

	(t (plist-get package-or-source :type))))

(defalias 'el-get-package-type #'el-get-package-method)

(defun el-get-package-types-alist (statuses &rest types)
  "Return an alist of package names that are of given types.

Only consider packages whose status is `member' of STATUSES,
which defaults to installed, required and removed.  Example:

  (el-get-package-types-alist \"installed\" 'http 'cvs)
"
  (loop for src in (apply 'el-get-list-package-names-with-status
			  (cond ((stringp statuses) (list statuses))
				((null statuses) '("installed" "required" "removed"))
				(t statuses)))
	for name = (el-get-as-symbol src)
	for type = (el-get-package-type name)
	when (or (null types) (memq 'all types) (memq type types))
	collect (cons name type)))


;;
;; package status --- a plist saved on a file, using symbols
;;
;; it should be possible to use strings instead, but in my tests it failed
;; miserably.
;;
(defun el-get-package-symbol (package-name)
  "Returns a symbol :package."
  (if (symbolp package-name) package-name
    (intern (format ":%s" package-name))))

(defun el-get-package-name (package-symbol)
  "Returns a string package"
  (if (symbolp package-symbol)
      (cadr (split-string (format "%s" package-symbol) ":"))
    package-symbol))

(defun el-get-read-all-packages-status ()
  "Return the current plist of packages status"
  (when (file-exists-p el-get-status-file)
    (car (with-temp-buffer
	   (insert-file-contents-literally el-get-status-file)
	   (read-from-string (buffer-string))))))

(defun el-get-read-package-status (package)
  "Return the current known status for given package."
  (plist-get (el-get-read-all-packages-status)
	     (el-get-package-symbol package)))

(defun el-get-save-package-status (package status)
  "Save given package status"
  (let ((p (el-get-package-symbol package))
	(s (el-get-read-all-packages-status)))
    (with-temp-file el-get-status-file
      (insert
       (format "%S" (if s (plist-put s p status)
		      `(,p ,status)))))))

(defun el-get-list-package-names-with-status (&rest status)
  "Return package names that are currently in given status"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	if (member s status) collect (el-get-package-name p)))

(defun el-get-read-package-with-status (action &rest status)
  "Read a package name in given status"
  (completing-read (format "%s package: " action)
                   (apply 'el-get-list-package-names-with-status status)))

(defun el-get-count-package-with-status (&rest status)
  "Return how many packages are currently in given status"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	if (member s status) sum 1))

(defun el-get-package-status (package &optional package-status-plist)
  "Return current status of package from given list"
  (let ((status-plist (or package-status-plist (el-get-read-all-packages-status))))
    (plist-get status-plist (el-get-package-symbol package))))

(defun el-get-extra-packages (&rest packages)
  "Return installed or required packages that are not in given package list"
  (let ((packages
	 ;; &rest could contain both symbols and lists
	 (loop for p in packages
	       when (listp p) append (mapcar 'el-get-as-symbol p)
	       else collect (el-get-as-symbol p))))
    (when packages
	(loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	      for x = (el-get-as-symbol (el-get-package-name p))
	      unless (member x packages)
	      unless (equal s "removed")
	      collect (list x s)))))

;;
;; Get list duplicates
;;
(defun el-get-duplicates (list)
  "Return duplicates found in list."
  (loop with dups and once
	for elt in list
	if (member elt once) collect elt into dups
	else collect elt into once
	finally return dups))


;;
;; User Interface, Interactive part
;;
(defun el-get-version ()
  "Message the current el-get version"
  (interactive)
  (message "el-get version %s" el-get-version))

(defun el-get-read-all-recipe-names ()
  "Return the list of all known recipe names.

This is useful to use for providing completion candidates for
package names."
  (mapcar 'el-get-source-name (el-get-read-all-recipes)))

(defun el-get-error-unless-package-p (package)
  "Raise an error if PACKAGE does not name a package that has a valid recipe."
  ;; check for recipe
  (let ((recipe (el-get-package-def package)))
    (unless recipe
      (error "el-get: package `%s' has no recipe" package))
    (unless (plist-member recipe :type)
      (error "el-get: package `%s' has incomplete recipe (no :type)" package))))

(defun el-get-package-is-installed (package)
  "Raise an error if PACKAGE is already installed"
  (string= "installed" (el-get-package-status (el-get-as-string package))))

(defun el-get-read-package-name (action &optional filtered)
  "Ask user for a package name in minibuffer, with completion.

Completions are offered from all known package names, after
removing any packages in FILTERED."
  (let ((packages   (el-get-read-all-recipe-names)))
    (completing-read (format "%s package: " action)
		     (set-difference packages filtered :test 'string=) nil t)))

(defun el-get-read-recipe-name (action)
  "Ask user for a recipe name, with completion from the list of known recipe files.

This function does not deal with `el-get-sources' at all."
  (completing-read (format "%s recipe: " action)
                   (el-get-read-all-recipe-names) nil))

(defun el-get-find-recipe-file (package &optional dir)
  "Find recipe file for PACKAGE.

If no recipe file exists for PACKAGE, create a new one in DIR,
which defaults to the first element in `el-get-recipe-path'."
  (interactive (list (el-get-read-recipe-name "Find or create")))
  (let* ((package-el (concat (el-get-as-string package) ".rcp"))
	 (recipe-file (or
		       ;; If dir was specified, open or create the
		       ;; recipe file in that directory.
		       (when dir (expand-file-name package-el dir))
		       ;; Next, try to find an existing recipe file anywhere.
		       (el-get-recipe-filename package)
		       ;; Lastly, create a new recipe file in the first
		       ;; directory in `el-get-recipe-path'
		       (expand-file-name package-el
                                         (car el-get-recipe-path)))))
    (find-file recipe-file)))

(defun el-get-save-and-kill (file)
  "Save and kill all buffers visiting the named FILE"
  (let (buf)
    (while (setq buf (find-buffer-visiting file))
      (with-current-buffer buf
        (save-buffer)
        (kill-buffer)))))

(defun el-get-ensure-byte-compilable-autoload-file (file)
  "If FILE doesn't already exist, create it as a byte-compilable
  autoload file (the default created by autoload.el has a local
  no-byte-compile variable that suppresses byte compilation)."
  ;; If we don't explicitly strip out the no-byte-compile variable,
  ;; autoload.el will create it on demand
  (unless (file-exists-p file)
    (write-region
     (replace-regexp-in-string ";; no-byte-compile: t\n" ""
			       (autoload-rubric file)) nil file)))

(defun el-get-load-fast (file)
  "Load the compiled version of FILE if it exists; else load FILE verbatim"
  (load (file-name-sans-extension file) nil (not el-get-verbose)))

(defun el-get-eval-autoloads ()
  "Evaluate the autoloads from the autoload file."
  (when (and el-get-generate-autoloads
             (file-exists-p el-get-autoload-file))
    (el-get-verbose-message "el-get: evaluating autoload file")
    (el-get-load-fast el-get-autoload-file)))

(defun el-get-update-autoloads ()
  "Regenerate, compile, and load any outdated packages' autoloads.

This function will run from a timer, and usually
shouldn't be invoked directly."

  (message "el-get: updating outdated autoloads")
  (setq el-get-autoload-timer nil) ;; Allow a new update to be primed

  (let ((outdated el-get-outdated-autoloads)
        ;; Generating autoloads runs theses hooks; disable then
        fundamental-mode-hook
        prog-mode-hook
        emacs-lisp-mode-hook
        ;; use dynamic scoping to set up our loaddefs file for
        ;; update-directory-autoloads
        (generated-autoload-file el-get-autoload-file))

    ;; make sure we can actually byte-compile it
    (el-get-ensure-byte-compilable-autoload-file generated-autoload-file)

    ;; clear the list early in case of errors
    (setq el-get-outdated-autoloads nil)

    (dolist (p outdated)
      (if (string= (el-get-package-status p) "installed")
          (apply 'update-directory-autoloads (el-get-load-path p))))

    (el-get-save-and-kill el-get-autoload-file)

    (when (file-exists-p el-get-autoload-file)
      (message "el-get: byte-compiling autoload file")
      (when el-get-byte-compile
        (el-get-byte-compile-file el-get-autoload-file))

      (el-get-eval-autoloads))))

(defconst el-get-load-suffix-regexp
  (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "\\'"))

(defun el-get-remove-autoloads (package)
  "Remove from `el-get-autoload-file' any autoloads associated
with the named PACKAGE"
  ;; Remove any compiled loaddefs file and schedule it for recompilation
  (el-get-schedule-autoload-update)

  (when (file-exists-p el-get-autoload-file)
    (with-temp-buffer ;; empty buffer to trick `autoload-find-destination'
      (let ((generated-autoload-file el-get-autoload-file)
            ;; Generating autoloads runs emacs-lisp-mode-hook; disable it
            emacs-lisp-mode-hook
            (autoload-modified-buffers (list (current-buffer))))
        (dolist (dir (el-get-load-path package))
          (when (file-directory-p dir)
            (dolist (f (directory-files dir t el-get-load-suffix-regexp))
              ;; this will clear out any autoloads associated with the file
              ;; `autoload-find-destination' signature has changed in emacs24.
              (if (> emacs-major-version 23)
                  (autoload-find-destination f (autoload-file-load-name f))
                (autoload-find-destination f)))))))
    (el-get-save-and-kill el-get-autoload-file)))

(defvar el-get-autoload-timer nil
  "Where the currently primed autoload timer (if any) is stored")

(defun el-get-want-autoloads-p (package)
  "Return t iff the given PACKAGE should have its autoloads
automatically generated by el-get"
  (let ((source (el-get-package-def package)))
    (or (not (plist-member source :autoloads))
        (eq (plist-get source :autoloads) t))))

(defun el-get-schedule-autoload-update ()
  "Autoloads need to be updated; delete any (now outdated)
compiled autoloads file and schedule the task to run later."
  (let ((elc (concat (file-name-sans-extension el-get-autoload-file) ".elc")))
    (when (file-exists-p elc)
      (delete-file elc)))
  (unless (or el-get-autoload-timer
              (not el-get-generate-autoloads))
    (setq el-get-autoload-timer
          (run-with-idle-timer 0 nil 'el-get-update-autoloads))))

(defun el-get-invalidate-autoloads ( &optional package )
  "Mark the named PACKAGE as needing new autoloads.  If PACKAGE
is nil, marks all installed packages as needing new autoloads."

  ;; Trigger autoload recomputation unless it's already been done
  (unless (or el-get-autoload-timer
              (not el-get-generate-autoloads))
    (setq el-get-autoload-timer
          (run-with-idle-timer 0 nil 'el-get-update-autoloads)))

  ;; Save the package names for later
  (mapc (lambda (p)
          (when (el-get-want-autoloads-p p)
            (add-to-list 'el-get-outdated-autoloads p)))
        (if package (list package)
	  (el-get-list-package-names-with-status "installed")))

  ;; If we're invalidating everything, try to start from a clean slate
  (unless package
    (ignore-errors
      (delete-file el-get-autoload-file)
      (delete-file
       (concat (file-name-sans-extension el-get-autoload-file) ".elc")))))

(defun el-get-funcall (func fname package)
  "`funcal' FUNC for PACKAGE and report about FNAME when `el-get-verbose'"
  (when (and func (functionp func))
      (el-get-verbose-message "el-get: Calling :%s function for package %s"
			      fname package)
      ;; don't forget to make some variables available
      (let (pdir (el-get-package-directory package))
	(funcall func))))

(defun el-get-init (package)
  "Make the named PACKAGE available for use.

Add PACKAGE's directory (or `:load-path' if specified) to the
`load-path', add any its `:info' directory to
`Info-directory-list', and `require' its `:features'.  Will be
called by `el-get' (usually at startup) for each installed package."
  (interactive (list (el-get-read-package-name "Init")))
  (el-get-verbose-message "el-get-init: %s" package)
  (condition-case err
      (let* ((source   (el-get-package-def package))
             (method   (el-get-package-method source))
             (loads    (el-get-as-list (plist-get source :load)))
             (autoloads (plist-get source :autoloads))
             (feats    (el-get-as-list (plist-get source :features)))
             (el-path  (el-get-as-list (el-get-load-path package)))
             (lazy     (plist-get source :lazy))
             (prepare  (plist-get source :prepare))
             (before   (plist-get source :before))
             (postinit (plist-get source :post-init))
             (after    (plist-get source :after))
             (pkgname  (plist-get source :pkgname))
             (library  (or (plist-get source :library) pkgname package))
             (pdir     (el-get-package-directory package)))

        ;; append entries to load-path and Info-directory-list
        (unless (member method '(elpa apt-get fink pacman))
          ;; append entries to load-path
          (dolist (path el-path)
            (el-get-add-path-to-list package 'load-path path))
          ;;  and Info-directory-list
          (el-get-install-or-init-info package 'init))

        (when el-get-byte-compile-at-init
          ;; If the package has been updated outside el-get, the .el files will be
          ;; out of date, so just check if we need to recompile them.
          ;;
          ;; when using el-get-update to update packages, though, there's no
          ;; need to byte compile at init.
          (el-get-byte-compile package))

        ;; load any autoloads file if needed
        (unless (eq autoloads t)
          (dolist (file (el-get-as-list autoloads))
            (el-get-load-fast file)))

        ;; first, the :prepare function, usually defined in the recipe
        (el-get-funcall prepare "prepare" package)

        ;; now call the :before user function
        (el-get-funcall before "before" package)

        ;; loads and feature are skipped when el-get-is-lazy
        (unless (or lazy el-get-is-lazy)
          ;; loads
          (dolist (file loads)
            (let ((pfile (concat pdir file)))
              (unless (file-exists-p pfile)
                (error "el-get could not find file '%s'" pfile))
              (el-get-verbose-message "el-get: load '%s'" pfile)
              (el-get-load-fast pfile)))

          ;; features, only ELPA will handle them on its own
          (unless (eq method 'elpa)
            ;; if a feature is provided, require it now
            (dolist (feat feats)
              (let ((feature (el-get-as-symbol feat)))
                (el-get-verbose-message "require '%s" feature)
                (require feature)))))

        ;; now handle the user configs and :post-init and :after functions
        (if (or lazy el-get-is-lazy)
            (let ((lazy-form
		   `(progn ,(when postinit (list 'funcall postinit))
			   (el-get-load-package-user-init-file ,package)
			   ,(when after (list 'funcall after)))))
              (eval-after-load library lazy-form))

          ;; el-get is not lazy here
          (el-get-funcall postinit "post-init" package)
	  (el-get-load-package-user-init-file package)
          (el-get-funcall after "after" package))

        ;; and call the global init hooks
        (run-hook-with-args 'el-get-post-init-hooks package)

        ;; return the package
        package)
    (debug error
     (el-get-installation-failed package err))))

(defun el-get-post-install-build (package)
  "Function to call after building the package while installing it."
  (el-get-invalidate-autoloads package)
  (el-get-init package)
  (el-get-save-package-status package "installed"))

(defun el-get-post-install (package)
  "Post install PACKAGE. This will get run by a sentinel."
  (let* ((sync     el-get-default-process-sync)
	 (hooks    (el-get-method (el-get-package-type package) :install-hook))
	 (commands (el-get-build-commands package)))

    ;; post-install is the right place to run install-hook
    (run-hook-with-args hooks package)

    ;; el-get-post-build will care about autoloads and initializing the
    ;; package, and will change the status to "installed"
    (el-get-build package commands nil sync 'el-get-post-install-build))
  (run-hook-with-args 'el-get-post-install-hooks package))

(defun el-get-do-install (package)
  "Install any PACKAGE for which you have a recipe."
  (el-get-error-unless-package-p package)
  (let* ((status   (el-get-read-package-status package))
	 (source   (el-get-package-def package))
	 (method   (el-get-package-method source))
	 (install  (el-get-method method :install))
	 (url      (plist-get source :url)))

    (when (string= "installed" status)
      (error "Package %s is already installed." package))

    (when (string= "required" status)
      (message "Package %s failed to install, removing it first." package)
      (el-get-remove package))

    ;; check we can install the package and save to "required" status
    (el-get-check-init)
    (el-get-save-package-status package "required")

    ;; and install the package now, *then* message about it
    (funcall install package url 'el-get-post-install)
    (message "el-get install %s" package)))

(defun el-get-post-update (package)
  "Post update PACKAGE. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (commands (el-get-build-commands package)))
    (el-get-build package commands nil el-get-default-process-sync
		  (lambda (package)
		    (el-get-init package)
		    ;; fix trailing failed installs
		    (when (string= (el-get-read-package-status package) "required")
		      (el-get-save-package-status package "installed"))
                    (run-hook-with-args 'el-get-post-update-hooks package)))))

(defun el-get-update (package)
  "Update PACKAGE."
  (interactive
   (list (el-get-read-package-with-status "Update" "required" "installed")))
  (el-get-error-unless-package-p package)
  (let* ((source   (el-get-package-def package))
	 (method   (el-get-package-method source))
	 (update   (el-get-method method :update))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))
    ;; update the package now
    (funcall update package url 'el-get-post-update)
    (message "el-get update %s" package)))

(defun el-get-update-all ()
  "Performs update of all installed packages."
  (interactive)
  (mapc 'el-get-update (el-get-list-package-names-with-status "installed")))

(defun el-get-self-update ()
  "Update el-get itself.  The standard recipe takes care of reloading the code."
  (interactive)
  (let ((el-get-default-process-sync t)
	(el-get-dir
	 (expand-file-name ".." (file-name-directory el-get-script))))
    (el-get-update "el-get")))

(defun el-get-post-remove (package)
  "Run the post-remove hooks for PACKAGE."
  (let* ((hooks   (el-get-method (el-get-package-method package) :remove-hook)))
    (run-hook-with-args hooks package)
    (run-hook-with-args 'el-get-post-remove-hooks package)))

(defun el-get-remove (package)
  "Remove any PACKAGE that is know to be installed or required."
  (interactive
   (list (el-get-read-package-with-status "Remove" "required" "installed")))
  (el-get-error-unless-package-p package)

  (let* ((source   (el-get-package-def package))
	 (method   (el-get-package-method source))
	 (remove   (el-get-method method :remove))
	 (url      (plist-get source :url)))
    ;; remove the package now
    (el-get-remove-autoloads package)
    (funcall remove package url 'el-get-post-remove)
    (el-get-save-package-status package "removed")
    (message "el-get remove %s" package)))

(defun el-get-cd (package)
  "Open dired in the package directory."
  (interactive
   (list (el-get-read-package-with-status "cd to" "required" "installed")))
  (el-get-error-unless-package-p package)
  (dired (el-get-package-directory package)))

(defun el-get-write-recipe (source dir &optional filename)
  "Given an SOURCE entry, write it to FILENAME"
  (let* (;; Replace a package name with its definition
	 (source (if (symbolp source) (el-get-read-recipe source) source))
	 ;; Autogenerate filename if unspecified
	 (filename (or filename (format "%s.rcp" (el-get-source-name source)))))
    ;; Filepath is dir/file
    (let ((filepath (format "%s/%s" dir filename)))
      (with-temp-file filepath
	(insert (prin1-to-string source))))))

(defun el-get-make-recipes (&optional dir)
  "Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe."
  (interactive "Dsave recipes in directory: ")
  (let* ((all (mapcar 'el-get-source-name (el-get-read-all-recipes)))
	 (new (loop for r in el-get-sources
		    when (and (not (symbolp r))
			      (not (member (el-get-source-name r) all)))
		    collect r)))
    (dolist (r new)
      (message "el-get: preparing recipe file for %s" (el-get-source-name r))
      (el-get-write-recipe r dir)))
  (dired dir))

;;
;; notify user with emacs notifications API (new in 24)
;;
(when (and (eq system-type 'darwin)
	   (file-executable-p el-get-growl-notify))
  (defun el-get-growl (title message)
    "Send a message to growl, that implements notifications for darwin"
    (let* ((name  "*growl*")
	   (proc
	    (start-process name name el-get-growl-notify title "-a" "Emacs")))
      (process-send-string proc (concat message "\n"))
      (process-send-eof proc))))

;;
;; Notification support is either the internal one provided by Emacs 24, or
;; the external growl one as defined above, or the one provided by the
;; add-on found on http://www.emacswiki.org/emacs/notify.el (there's a
;; recipe) for older Emacs versions users
;;
(defun el-get-notify (title message)
  "Notify the user using either the dbus based API or the `growl' one"
  (if (fboundp 'dbus-register-signal)
      ;; avoid a bug in Emacs 24.0 under darwin
      (require 'notifications nil t)
    ;; else try notify.el, there's a recipe for it
    (unless (fboundp 'notify)
      (when (featurep 'notify)
	(require 'notify))))

  (cond ((fboundp 'notifications-notify) (notifications-notify :title title
							       :body message))
	((fboundp 'notify)               (notify title message))
	((fboundp 'el-get-growl)         (el-get-growl title message))
	(t                               (message "%s: %s" title message))))

(defun el-get-post-install-notification (package)
  "Notify the PACKAGE has been installed."
  (el-get-notify (format "%s installed" package)
		 "This package has been installed successfully by el-get."))
(add-hook 'el-get-post-install-hooks 'el-get-post-install-notification)

(defun el-get-post-update-notification (package)
  "Notify the PACKAGE has been updated."
  (el-get-notify (format "%s updated" package)
		 "This package has been updated successfully by el-get."))
(add-hook 'el-get-post-update-hooks 'el-get-post-update-notification)

(defun el-get-post-remove-notification (package)
  "Notify the PACKAGE has been removed."
  (el-get-notify (format "%s removed" package)
		 "This package has been removed successfully by el-get."))
(add-hook 'el-get-post-remove-hooks 'el-get-post-remove-notification)

(defun el-get-post-error-notification (package info)
  "Notify the PACKAGE has failed to install."
  (el-get-notify (format "%s failed to install" package)
		 (format "%s" info)))
(add-hook 'el-get-post-error-hooks 'el-get-post-error-notification)

;;
;; Emacs `message' notifications
;;
(defun el-get-post-init-message (package)
  "After PACKAGE init is done, just message about it"
  (el-get-verbose-message "el-get initialized package %s" package))
(add-hook 'el-get-post-init-hooks 'el-get-post-init-message)

(defun el-get-post-update-message (package)
  "After PACKAGE update is done, message about it"
  (el-get-verbose-message "el-get updated package %s" package))
(add-hook 'el-get-post-update-hooks 'el-get-post-update-message)

(defun el-get-post-remove-message (package)
  "After PACKAGE remove is done, message about it"
  (el-get-verbose-message "el-get removed package %s" package))
(add-hook 'el-get-post-remove-hooks 'el-get-post-remove-message)

(defun el-get-post-error-message (package info)
  "After PACKAGE fails to install, just message about it"
  (el-get-verbose-message "el-get failed to initialize package %s" package))
(add-hook 'el-get-post-error-hooks 'el-get-post-error-message)

;;
;; Description of packages.  (Code based on `describe-function').
;;
(define-button-type 'el-get-help-package-def
  :supertype 'help-xref
  'help-function (lambda (package) (find-file (el-get-recipe-filename package)))
  'help-echo (purecopy "mouse-2, RET: find package's recipe"))

(define-button-type 'el-get-help-install
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                          (format "Do you really want to install `%s'? "
                                  package))
                       (el-get-install package)))
  'help-echo (purecopy "mouse-2, RET: install package"))

(define-button-type 'el-get-help-remove
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                        (format "Do you really want to uninstall `%s'? "
                                package))
                       (el-get-remove package)))
  'help-echo (purecopy "mouse-2, RET: remove package"))

(define-button-type 'el-get-help-update
  :supertype 'help-xref
  'help-function (lambda (package)
                   (when (y-or-n-p
                        (format "Do you really want to update `%s'? "
                                package))
                       (el-get-update package)))
  'help-echo (purecopy "mouse-2, RET: update package"))

(define-button-type 'el-get-help-describe-package
  :supertype 'help-xref
  'help-function #'el-get-describe
  'help-echo (purecopy "mouse-2, RET: describe package"))

(defun el-get-describe-princ-button (label regex type &rest args)
  "Princ a new button with label LABEL.

The LABEL is made clickable by calling `help-xref-button' for a backwards
matching REGEX with TYPE and ARGS as parameter."
  (princ label)
  (with-current-buffer standard-output
    (save-excursion
      (re-search-backward regex nil t)
      (apply #'help-xref-button 1 type args))))

(defun el-get-describe-1 (package)
  (let* ((psym (el-get-as-symbol package))
         (pname (symbol-name psym))
         (status (el-get-read-package-status package))
         (def (el-get-package-def pname))
         (name (plist-get def :name))
         (website (plist-get def :website))
         (descr (plist-get def :description))
         (type (plist-get def :type))
         (url (plist-get def :url))
         (depends (plist-get def :depends)))
    (princ (format "%s is an `el-get' package. It is currently %s " name
                   (if status status "not installed")))

    (cond
     ((string= status "installed")
      (el-get-describe-princ-button "[update]" "\\[\\([^]]+\\)\\]"
                                    'el-get-help-update package)
      (el-get-describe-princ-button "[remove]" "\\[\\([^]]+\\)\\]"
                                    'el-get-help-remove package))
     ((string= status "required")
      (el-get-describe-princ-button "[update]" "\\[\\([^]]+\\)\\]"
                                    'el-get-help-update package))
     (t
      (el-get-describe-princ-button "[install]" "\\[\\([^]]+\\)\\]"
                                    'el-get-help-install package)))
    (princ ".\n\n")

    (when website
      (el-get-describe-princ-button (format "Website: %s\n" website)
                                    ": \\(.+\\)" 'help-url website))
    (when descr
      (princ (format "Description: %s\n" descr)))
    (when depends
      (if (listp depends)
          (progn
            (princ "Dependencies: ")
            (loop for i in depends
                  do (el-get-describe-princ-button
                      (format "`%s'" i) "`\\([^`']+\\)"
                      'el-get-help-describe-package i)))
        (princ "Dependency: ")
        (el-get-describe-princ-button
         (format "`%s'" depends) "`\\([^`']+\\)"
         'el-get-help-describe-package depends))
      (princ ".\n"))
    (princ (format "The default installation method is %s %s\n\n" type
                   (if url (format "from %s" url) "")))
    (princ "Full definition")
    (let ((file (el-get-recipe-filename package)))
      (if (not file)
          (princ ":\n")
        (el-get-describe-princ-button (format " in `%s':\n" file)
                                      "`\\([^`']+\\)"
                                      'el-get-help-package-def package)))
    (prin1 def)))

(defun el-get-describe (package)
  "Generate a description for PACKAGE."
  (interactive
   (list
    (el-get-read-package-name "Describe")))

  (if (null package)
      (message "You didn't specify a package")
    (help-setup-xref (list #'el-get-describe package)
		     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (el-get-describe-1 package)
        (with-current-buffer standard-output
          (buffer-string))))))

;;
;; Package Menu
;;

(defvar el-get-package-menu-mode-hook nil
  "Hooks to run after el-get package menu init.")

(defvar el-get-package-menu-mode-map nil
  "Keymap for el-get-package-menu-mode")

(defvar el-get-package-menu-sort-key nil
  "sort packages by key")

(defun el-get-package-menu-get-package-name ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". \\([^ \t]*\\)")
		(match-string 1))))

(defun el-get-package-menu-get-status ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ". [^ \t]*[ \t]*\\([^ \t\n]*\\)")
		(match-string 1))))

(defun el-get-package-menu-mark (what)
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line)
	  (setq buffer-read-only t))))

(defun el-get-package-menu-mark-install ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "available")
		  (string= (el-get-package-menu-get-status) "removed"))
	  (el-get-package-menu-mark "I")))

(defun el-get-package-menu-mark-update ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "installed")
		  (string= (el-get-package-menu-get-status) "required"))
	  (el-get-package-menu-mark "U")))

(defun el-get-package-menu-mark-delete ()
  (interactive)
  (if (or (string= (el-get-package-menu-get-status) "installed")
		  (string= (el-get-package-menu-get-status) "required"))
	  (el-get-package-menu-mark "D")))

(defun el-get-package-menu-mark-unmark ()
  (interactive)
  (el-get-package-menu-mark " "))

(defun el-get-package-menu-revert ()
  (interactive)
  (let ((current-point (point)))
	(el-get-package-menu)
	(goto-char current-point)
	(beginning-of-line)))

(defun el-get-package-menu-execute ()
  (interactive)
  (let ((current-point (point)))
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((command (char-after))
			(package-name (el-get-package-menu-get-package-name)))
		(cond
		 ((eq command ?I)
		  (message "Installing %s..." package-name)
		  (el-get-install package-name)
		  (message "Installing %s...done" package-name))
		 ((eq command ?U)
		  (message "Updating %s..." package-name)
		  (el-get-update package-name)
		  (message "Updating %s...done" package-name))
		 ((eq command ?D)
		  (message "Deleting %s..." package-name)
		  (el-get-remove package-name)
		  (message "Deleting %s..." package-name))))
	  (forward-line))
	(el-get-package-menu-revert)
	(goto-char current-point)
	(beginning-of-line)))

(defun el-get-package-menu-describe ()
  (interactive)
  (el-get-describe (el-get-package-menu-get-package-name)))

(defun el-get-package-menu-quick-help ()
  (interactive)
  (message "n-ext, p-revious, i-nstall, u-pdate, d-elete, SPC-unmark, g-revert, x-execute, ?-package describe, h-elp, q-uit"))

(unless el-get-package-menu-mode-map
  (setq el-get-package-menu-mode-map (make-keymap))
  (suppress-keymap el-get-package-menu-mode-map)
  (define-key el-get-package-menu-mode-map "n" 'next-line)
  (define-key el-get-package-menu-mode-map "p" 'previous-line)
  (define-key el-get-package-menu-mode-map "i" 'el-get-package-menu-mark-install)
  (define-key el-get-package-menu-mode-map "u" 'el-get-package-menu-mark-update)
  (define-key el-get-package-menu-mode-map "d" 'el-get-package-menu-mark-delete)
  (define-key el-get-package-menu-mode-map " " 'el-get-package-menu-mark-unmark)
  (define-key el-get-package-menu-mode-map "g" 'el-get-package-menu-revert)
  (define-key el-get-package-menu-mode-map "x" 'el-get-package-menu-execute)
  (define-key el-get-package-menu-mode-map "?" 'el-get-package-menu-describe)
  (define-key el-get-package-menu-mode-map "h" 'el-get-package-menu-quick-help)
  (define-key el-get-package-menu-mode-map "q" 'quit-window))

(defun el-get-package-menu-mode ()
  "Major mode for browsing a list of packages."
  (kill-all-local-variables)
  (use-local-map el-get-package-menu-mode-map)
  (setq major-mode 'el-get-package-menu-mode)
  (setq mode-name "Package-Menu")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (if (fboundp 'run-mode-hooks)
	  (run-mode-hooks 'el-get-package-menu-mode-hook)
	(run-hooks 'el-get-package-menu-mode-hook)))

(defun el-get-print-package (package-name status desc)
  (let ((face
		 (cond
		  ((string= status "installed")
		   'font-lock-comment-face)
		  ((string= status "required")
		   'font-lock-keyword-face)
		  ((string= status "removed")
		   'font-lock-string-face)
		  (t
		   (setq status "available")
		   'default))))
	(indent-to 2 1)
	(insert (propertize package-name 'font-lock-face face))
	(indent-to 30 1)
	(insert (propertize status 'font-lock-face face))
	(when desc
	  (indent-to 41 1)
	  (insert (propertize
			   (replace-regexp-in-string "\n" " " desc)
			   'font-lock-face face)))
	(insert "\n")))

(defun el-get-list-all-packages ()
  (with-current-buffer (get-buffer-create "*el-get packages*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(let ((packages (el-get-read-all-recipes)))
	  (let ((selector (cond
					   ((string= el-get-package-menu-sort-key "Status")
						#'(lambda (package)
							(let ((package-name (el-get-as-string (plist-get package :name))))
							  (el-get-package-status package-name))))
					   ((string= el-get-package-menu-sort-key "Description")
						#'(lambda (package)
							(plist-get package :description)))
					   (t
						#'(lambda (package)
							(el-get-as-string (plist-get package :name)))))))
		(setq packages
			  (sort packages
					(lambda (left right)
					  (let ((vleft (funcall selector left))
							(vright (funcall selector right)))
						(string< vleft vright))))))
	  (mapc (lambda (package)
			  (let ((package-name (el-get-as-string (plist-get package :name))))
				(el-get-print-package package-name
									  (el-get-package-status package-name)
									  (plist-get package :description))))
			packages))
	(goto-char (point-min))
	(current-buffer)))

(defun el-get-package-menu-sort-by-column (&optional e)
  "Sort the package menu by the last column clicked on."
  (interactive (list last-input-event))
  (if e (mouse-select-window e))
  (let* ((pos (event-start e))
		 (obj (posn-object pos))
		 (col (if obj
				  (get-text-property (cdr obj) 'column-name (car obj))
				(get-text-property (posn-point pos) 'column-name))))
    (setq el-get-package-menu-sort-key col)
	(el-get-package-menu)))

(defvar el-get-package-menu-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'el-get-package-menu-sort-by-column)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for package menu sort buttons.")

(defun el-get-package-menu ()
  (with-current-buffer (el-get-list-all-packages)
	(el-get-package-menu-mode)
	(setq header-line-format
		  (mapconcat
		   (lambda (pair)
			 (let ((column (car pair))
				   (name (cdr pair)))
			   (concat
				;; Insert a space that aligns the button properly.
				(propertize " " 'display (list 'space :align-to column)
							'face 'fixed-pitch)
				;; Set up the column button.
				(propertize name
							'column-name name
							'help-echo "mouse-1: sort by column"
							'mouse-face 'highlight
							'keymap el-get-package-menu-sort-button-map))))
		   '((2 . "Package")
			 (30 . "Status")
			 (41 . "Description"))
		   ""))
	(pop-to-buffer (current-buffer))))

(defun el-get-list-packages ()
  "Display a list of packages."
  (interactive)
  (el-get-package-menu))


;;
;; User Interface, Non Interactive part
;;
(defun el-get-init-and-install (&optional packages)
  "Install \"required\" packages, init \"installed\" packages.

When PACKAGES is non-nil, only process entries from this list.
Those packages from the list we don't know the status of are
considered \"required\"."
  (let* ((required    (el-get-list-package-names-with-status "required"))
	 (installed   (el-get-list-package-names-with-status "installed"))
	 (to-init     (if packages
			  (loop for p in packages
				when (member (el-get-as-string p) installed)
				collect (el-get-as-string p))
			installed))
	 (init-deps   (loop for p in to-init
			    append (mapcar 'el-get-as-string
					   (el-get-dependencies
					    (el-get-as-symbol p)))))
	 (to-install  (if packages
			  (loop for p in packages
				unless (member (el-get-as-string p) to-init)
				collect (el-get-as-string p))
			required))
	 done)
    (el-get-verbose-message "el-get-init-and-install: install %S" to-install)
    (el-get-verbose-message "el-get-init-and-install: init %S" to-init)
    (el-get-verbose-message "el-get-init-and-install: deps %S" init-deps)

    (loop for p in to-install do (el-get-install p) collect p into done)
    (loop for p in init-deps  do (el-get-init p)    collect p into done)
    (loop for p in to-init
	  unless (member p done) do (el-get-init p) collect p into done)
    done))

(defun el-get (&optional sync &rest packages)
  "Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

When SYNC is 'wait, then `el-get' will enter a wait-loop and only
let you use Emacs once it has finished with its job. That's
useful an option to use in your `user-init-file'. Note that each
package in the list gets installed in parallel with this option.

Please note that the `el-get-init' part of `el-get' is always
done synchronously, so you will have to wait here. There's
`byte-compile' support though, and the packages you use are
welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered."
  (unless (or (null sync)
	      (member sync '(sync wait)))
    (error "el-get sync parameter should be either nil, sync or wait"))

  ;; If there's no autoload file, everything needs to be regenerated.
  (if (not (file-exists-p el-get-autoload-file)) (el-get-invalidate-autoloads))

  ;; Autoloads path are relative to el-get-dir, so add it to load-path
  (add-to-list 'load-path (file-name-as-directory el-get-dir))

  (let ((previously-installing (el-get-currently-installing-packages))
        (progress (and (eq sync 'wait)
                        (make-progress-reporter
			 "Waiting for `el-get' to complete... "
			 0 100 0)))
         (el-get-default-process-sync sync))

    ;; keep the result of `el-get-init-and-install' to return it even in the
    ;; 'wait case
    (prog1
	(let ((packages
	       ;; (el-get 'sync 'a 'b my-package-list)
	       (loop for p in packages when (listp p) append p else collect p)))
	  (el-get-init-and-install packages))

      ;; el-get-do-install is async, that's now ongoing.
      (when progress
        (let* ((newly-installing
               (set-difference (el-get-currently-installing-packages)
                               previously-installing))
              (still-installing newly-installing))

          (while (> (length still-installing) 0)
            (sleep-for 0.2)
            (setq still-installing (delete-if-not 'el-get-currently-installing-p still-installing))
            (progress-reporter-update
             progress
             (/ (* 100.0 (- newly-installing still-installing)) newly-installing)))
        (progress-reporter-done progress)))

      ;; unless we have autoloads to update, just load them now
      (unless el-get-outdated-autoloads
	(el-get-eval-autoloads)))))

(provide 'el-get)

;;; el-get.el ends here


;; Local Variables:
;; eval: (require 'whitespace)
;; whitespace-line-column:80
;; whitespace-style:(face trailing lines-tail)
;; eval: (set-face-attribute 'whitespace-tab nil :background "red1" :foreground "yellow" :weight 'bold)
;; eval: (set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
;; eval: (whitespace-mode)
;; End:
