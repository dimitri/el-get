;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2012 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Version: 4.1
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs svn darcs hg
;;           apt-get fink pacman http http-tar emacswiki
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.md file from the same distribution

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
;;
;; In addition to the version, you can also get the exact git revision
;; by running M-x `el-get-self-checksum'. You should provide this
;; checksum when seeking support or reporting a bug, so that the
;; developers will know exactly which version you are using.

;;; Change Log:
;;
;;  5.1 - 2014-01-11 - Rolling release
;;
;;   - "master" branch is now considered forever stable
;;   - many bug fixes
;;   - many new recipes
;;
;;  4.1 - 2012-08-28 - To infinity, and beyond!
;;
;;   - code refactoring
;;   - fix dependency tracking at install and init times
;;   - fix autoloading so that it happens before notifying install is done
;;   - add some tests
;;   - deprecate package.el from the old days, only include the Emacs24 one
;;   - implement :builtin property (useful for dealing with package.el)
;;   - fix recipes :build commands, must be either lists of strings or expr
;;   - add support for el-get-reload and do that at update time
;;   - implement :checksum property for http kinds of files
;;   - Add new command el-get-reinstall
;;   - implement :checkout property for git, fossil, hg, and http methods
;;   - implement :shallow property for git packages
;;   - add support for auto-building of ELPA recipes
;;   - implement :submodule property for git packages (allow bypassing them)
;;   - New package types: github, github-tar and zip, emacsmirror, fossil, etc
;;   - Support for installing CVS packages through non-transparent
;;     http proxy servers
;;   - `el-get-update-all' now prompts before updating packages
;;   - new status file format, with a cache
;;   - plenty new recipes
;;   - fetch emacswiki recipes at install time by default
;;   - new documentation, in proper info format
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


(require 'el-get-core)                  ; core facilities used everywhere
(require 'el-get-custom)                ; user tweaks and `el-get-sources'
(require 'el-get-methods)               ; support for `el-get-methods', backends
(require 'el-get-recipes)               ; support for dealing with recipes
(require 'el-get-status)                ; support for dealing with status
(require 'el-get-build)                 ; building packages
(require 'el-get-byte-compile)          ; byte compiling in a subprocess
(require 'el-get-dependencies)          ; topological-sort of package dep graph
(require 'el-get-notify)                ; notification support (dbus, growl...)
(require 'el-get-list-packages)         ; menu and `el-get-describe' facilities
(require 'el-get-autoloading)           ; manages updating el-get's loaddefs.el

;; optional features
(let* ((el-get-dir (file-name-directory load-file-name))
       (bundle (expand-file-name "el-get-bundle" el-get-dir)))
  (autoload 'el-get-bundle-el-get bundle)
  (autoload 'el-get-bundle bundle nil nil 'macro)
  (autoload 'el-get-bundle! bundle nil nil 'macro))

(defvar el-get-next-packages nil
  "List of packages to install next, used when dealing with dependencies.")

(defun el-get-installation-failed (package signal-data)
  "Run all the failure hooks for PACKAGE and `signal' the car and cdr of SIGNAL-DATA."
  (run-hook-with-args 'el-get-post-error-hooks package signal-data)
  (signal (car signal-data) (cdr signal-data)))


;;
;; User Interface, Interactive part
;;
;;;###autoload
(defun el-get-version ()
  "Message the current el-get version"
  (interactive)
  (let ((version
         (let ((default-directory (file-name-directory el-get-script)))
           (concat el-get-version "."
                   (shell-command-to-string
                    "git --no-pager log -n1 --format=format:%h")))))
    (kill-new version)
    (message "el-get version %s" version)))

(defun el-get-read-all-recipe-names ()
  "Return the list of all known recipe names.

This is useful to use for providing completion candidates for
package names."
  (delete-dups (append (mapcar #'el-get-source-name el-get-sources)
                       (el-get-all-recipe-file-names))))

(defun el-get-error-unless-package-p (package)
  "Raise an error if PACKAGE does not name a package that has a valid recipe."
  ;; check for recipe
  (let ((recipe (el-get-package-def package)))
    (unless recipe
      (error "el-get: package `%s' has no recipe" package))
    (unless (plist-member recipe :type)
      (error "el-get: package `%s' has incomplete recipe (no :type)" package))))

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
                   (el-get-read-all-recipe-names) nil nil (thing-at-point 'symbol t)))

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

(defun el-get-eval-after-load (package form)
  "Like `eval-after-load', but first arg is an el-get package name."
  (let* ((package  (el-get-as-symbol package))
         (library  (el-get-package-effective-library package)))
    (el-get-verbose-message "Using library `%s' for `eval-after-load' for package `%s'"
                            library package)
    (eval-after-load (el-get-as-string library) form)))
(put 'el-get-eval-after-load 'lisp-indent-function
     (get 'eval-after-load 'lisp-indent-function))

(defun el-get-run-package-support (form fname package)
  "`eval' FORM for PACKAGE and report about FNAME when `el-get-verbose'"
  (let* (;; Auto-strip quoting from form before doing anything else
         (form (if (equal (car-safe form) 'quote)
                   (prog1 (eval form)
                     (warn "The :%s form for package %s is quoted unnecessarily."
                           fname package))
                 form))
         ;; Allow either function (symbol or lambda) or single lisp form
         (form
          (cond
           ;; Nil means do nothing
           ((null form) nil)
           ;; A symbol is the name of a function to call
           ((symbolp form)
            ;; Convert it to a quoted call to that function
            (prog1 `(,form)
              (warn "The :%s form for package %s uses the old-style function form instead of a lisp form. The value should be changed from `%S' to `%S'"
                    fname package form `(,form))
              (unless (symbol-function form)
                (warn "The function %s, which is called in the :%s form for package %s, does not seem to be defined. Calling it will probably fail."
                      form fname package))))
           ;; A non-symbol function (like a lambda or something)
           ((functionp form)
            ;; Convert it to a quoted call to that function
            (prog1 `(,form)
              (if (and (listp form)
                       (equal (subseq form 0 2) '(lambda ())))
                  ;; It's a zero-arg function, so it can be trivially
                  ;; rewritten as a progn. Inform the user of such.
                  (warn "The :%s form for package %s uses the old-style lambda form instead of a lisp form. The leading \"(lambda ()\" should be replaced with \"(progn\"."
                        fname package)
                ;; Otherwise, provide a less informative warning
                (warn "The :%s form for package %s uses the old-style function form instead of a lisp form."
                      fname package))))
           ;; A list is interpreted as a single lisp form to be passed
           ;; directly to `eval'.
           ((listp form) form)
           ;; Anything else is an error
           (t (error "Unknown :%s form for package %s: `%S'"
                     fname package form)))))
    (when form
      (assert (listp form))
      (el-get-verbose-message "el-get: Evaluating :%s form for package %s"
                              fname package)
      ;; don't forget to make some variables available
      (let* ((pdir (el-get-package-directory package))
             (default-directory pdir))
        (eval form)))))

(defun el-get-lazy-run-package-support (form fname package)
  "Like `el-get-run-package-support', but using `eval-after-load' to wait until PACKAGE is loaded."
  (el-get-eval-after-load package
    `(el-get-run-package-support ',form ',fname ',package)))


(defvar el-get-activated-list nil
  "List of packages initialized by el-get.")

(defun el-get-init (package)
  "Make the named PACKAGE available for use, first initializing any
   dependency of the PACKAGE."
  (interactive (progn
                 (el-get-clear-status-cache)
                 (list (el-get-read-package-with-status "Init" "installed"))))
  (el-get-verbose-message "el-get-init: %s" package)
  (let* ((init-deps   (el-get-dependencies (el-get-as-symbol package))))
    (el-get-verbose-message "el-get-init: " init-deps)
    (loop for p in init-deps do (el-get-do-init p) collect p)))

(defun el-get-do-init (package &optional package-status-alist)
  "Make the named PACKAGE available for use.

Add PACKAGE's directory (or `:load-path' if specified) to the
`load-path', add any its `:info' directory to
`Info-directory-list', and `require' its `:features'.  Will be
called by `el-get' (usually at startup) for each installed package."
  (declare (advertised-calling-convention (package) "Feb 2015"))
    (let ((psym (el-get-as-symbol package)))
      (when (and (not (eq psym 'el-get)) ; el-get recipe handles reloading
                 (memq psym (bound-and-true-p package-activated-list))
                 (not (memq psym el-get-activated-list))
                 (package-installed-p psym)
                 (not (eq 'elpa (el-get-package-method package))))
        (lwarn 'el-get :warning
               "The package `%s' has already been loaded by
package.el, attempting to load el-get version instead. To avoid
this warning either uninstall one of the el-get or package.el
version of %s, or call `el-get' before `package-initialize' to
prevent package.el from loading it."  package package)))
  (when el-get-auto-update-cached-recipes
    (el-get-merge-properties-into-status package 'init :noerror t))
  (condition-case err
      (let* ((el-get-sources (el-get-package-status-recipes))
             (source   (el-get-read-package-status-recipe package))
             (method   (el-get-package-method source))
             (loads    (el-get-as-list (plist-get source :load)))
             (autoloads (plist-get source :autoloads))
             (feats    (el-get-as-list (plist-get source :features)))
             (el-path  (el-get-as-list (el-get-load-path package)))
             (lazy     (el-get-plist-get-with-default source :lazy
                         el-get-is-lazy))
             (prepare  (plist-get source :prepare))
             (before   (plist-get source :before))
             (postinit (plist-get source :post-init))
             (after    (plist-get source :after))
             (pkgname  (plist-get source :pkgname))
             (pdir     (el-get-package-directory package)))

        (el-get-error-unless-required-emacs-version source)

        ;; a builtin package initialisation is about calling recipe and user
        ;; code only, no load-path nor byte-compiling support needed here.
        (unless (eq method 'builtin)
          ;; append entries to load-path and Info-directory-list
          (unless (member method '(apt-get fink pacman))
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
              (el-get-load-fast file))))

        ;; first, the :prepare function, usually defined in the recipe
        (el-get-run-package-support prepare "prepare" package)

        ;; now call the :before user function
        (el-get-run-package-support before "before" package)

        ;; loads and feature are skipped when el-get-is-lazy
        (unless lazy
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

        (let ((el-get-maybe-lazy-runsupp
               (if lazy
                   #'el-get-lazy-run-package-support
                 #'el-get-run-package-support))
              (maybe-lazy-eval
               (if lazy
                   (apply-partially 'el-get-eval-after-load package)
                 'eval)))
          (funcall el-get-maybe-lazy-runsupp
                   postinit "post-init" package)
          (funcall maybe-lazy-eval `(el-get-load-package-user-init-file ',package))
          (funcall el-get-maybe-lazy-runsupp
                   after "after" package))
        ;; if any elpa packages are installed they already `require'd
        ;; `package'.
        (when (featurep 'package)
          ;; tell elpa that this package has been activated, so it
          ;; doesn't try to activate it's own package instead.
          (push (el-get-as-symbol package) package-activated-list))
        (push (el-get-as-symbol package) el-get-activated-list))
    (debug err
           (el-get-installation-failed package err)))
  ;; and call the global init hooks
  (run-hook-with-args 'el-get-post-init-hooks package)

  ;; return the package
  package)


;;;###autoload
(defun el-get-install (package)
  "Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol."
  (interactive (progn
                 (el-get-clear-status-cache)
                 (list (el-get-read-package-name "Install"))))
  (setq el-get-next-packages (el-get-dependencies (el-get-as-symbol package)))
  (el-get-verbose-message "el-get-install %s: %S" package el-get-next-packages)
  (add-hook 'el-get-post-init-hooks 'el-get-install-next-packages)
  ;; Start the chain of dependency installation
  (el-get-install-next-packages))

(defun el-get-install-next-packages (&rest ignored)
  "Run as part of `el-get-post-init-hooks' when dealing with dependencies."
  (let ((package (pop el-get-next-packages)))
    (el-get-verbose-message "el-get-install-next-packages: %s" package)
    (if package
        ;; el-get-do-install will either init the package, installing it
        ;; first only when necessary to do so
        (el-get-do-install (el-get-as-string package))
      ;; no more packages to install in the dependency walk, clean up
      (remove-hook 'el-get-post-init-hooks 'el-get-install-next-packages))))

(defun el-get-post-install-build (package)
  "Function to call after building the package while installing it."
  (el-get-save-package-status package "installed")
  (el-get-invalidate-autoloads package) ; that will also update them
  (el-get-do-init package)
  (run-hook-with-args 'el-get-post-install-hooks package))

(defun el-get-verify-checksum (package)
  (let* ((type             (el-get-package-type package))
         (checksum         (plist-get (el-get-package-def package) :checksum))
         (compute-checksum (el-get-method type :compute-checksum)))

    (when (and checksum (not compute-checksum))
      (error
       "Checksum verification of package %s is not supported with method %s."
       package type))
    (when compute-checksum
      (let ((computed (funcall compute-checksum package)))
        (if checksum
            (if (equal computed (el-get-as-string checksum))
                (el-get-verbose-message "el-get: package %s passed checksum with \"%s\"."
                                        package computed)
              (error "Checksum verification failed. Required: \"%s\", actual: \"%s\"."
                     checksum computed))
          (el-get-verbose-message "el-get: pakage %s checksum is %s."
                                  package computed))))))

(defun el-get-post-install (package)
  "Post install PACKAGE. This will get run by a sentinel."
  (let* ((sync             el-get-default-process-sync)
         (type             (el-get-package-type package))
         (hooks            (el-get-method type :install-hook))
         (commands         (el-get-build-commands package)))

    ;; check the checksum of the package here, as early as possible
    (el-get-verify-checksum package)

    ;; post-install is the right place to run install-hook
    (run-hook-with-args hooks package)

    ;; el-get-post-build will care about autoloads and initializing the
    ;; package, and will change the status to "installed"
    (if (and (eq (el-get-package-method package) 'builtin)
             (plist-get (el-get-package-def package) :builtin))
        ;; Do not run :build/:info if package is :builtin.  Run post-install directly
        (el-get-post-install-build package)
      (el-get-build package commands nil sync 'el-get-post-install-build))))

(defun el-get-do-install (package)
  "Install any PACKAGE for which you have a recipe."
  (el-get-error-unless-package-p package)
  (if (el-get-package-is-installed package)
      (progn
        (el-get-verbose-message "el-get: `%s' package is already installed" package)
        (el-get-do-init package))
    (let* ((status   (el-get-read-package-status package))
           (source   (el-get-package-def package))
           (method   (el-get-package-method source))
           (install  (el-get-method method :install))
           (url      (plist-get source :url))
           (pdir     (el-get-package-directory package)))

      (el-get-error-unless-required-emacs-version source)

      (cond ((string= "installed" status)
             (error "Package %s is already installed." package))
            ((string= "required" status)
             (message "Package %s previously failed to install, removing it first." package)
             (el-get-remove package))
            ((file-exists-p pdir)
             (message "Package %s has an install dir but is not known to be installed. Removing it so we can install a known version." package)
             (el-get-remove package)))

      ;; check we can install the package and save to "required" status
      (el-get-check-init)
      (el-get-save-package-status package "required")

      ;; and install the package now, *then* message about it
      (funcall install package url 'el-get-post-install)
      (message "el-get install %s" package))))

(defun el-get-reload (package &optional package-status-alist)
  "Reload PACKAGE."
  (interactive
   (progn
     (el-get-clear-status-cache)
     (list (el-get-read-package-with-status "Reload" "installed"))))
  (declare (advertised-calling-convention (package) "Feb 2015"))
  (el-get-verbose-message "el-get-reload: %s" package)
  (el-get-with-status-sources ()
    (let* ((all-features features)
           (pdir (el-get-package-directory package))
           (package-features (el-get-package-features pdir))
           (package-files (el-get-package-files pdir))
           (other-features
            (remove-if (lambda (x) (memq x package-features)) all-features)))
      (unwind-protect
          (progn
            ;; We cannot let-bind `features' here, becauses the changes
            ;; made by `el-get-init' must persist.
            (setq features other-features)
            ;; Reload all loaded files in package dir if they still
            ;; exist.
            (loop for file in package-files
                  ;; We convert errors to warnings here, because some
                  ;; files don't like being loaded more than once in a
                  ;; session. Example: "cedet-remove-builtin.el" from
                  ;; CEDET.
                  do (condition-case e
                         (load file 'noerror)
                       (error (warn "Error while reloading file %s in package %s: %S\n\n This package may require a restart of emacs to complete the update process."
                                    file package (cdr e)))))
            ;; Redo package initialization
            (el-get-init package)
            ;; Reload all features provided by the package. This ensures
            ;; that autoloaded packages (which normally don't load
            ;; anything until one of their entry points is called) are
            ;; forced to reload immediately if they were already loaded.
            (loop for f in package-features
                  do (require f nil 'noerror)))
        ;; We have to add all the removed features back in no matter
        ;; what, or else we would be lying about what has been loaded.
        ;; This covers the corner case where an updated package no
        ;; longer provides a certain feature. Technically that feature
        ;; is still provided, so not adding it back would be wrong.
        (let ((missing-features
               (remove-if (lambda (x) (memq x features)) package-features)))
          (when missing-features
            (warn "Adding %S back onto features, because the reloaded package did not provide them."
                  missing-features)
            (setq features (append missing-features features))))))))

(defun el-get-post-update-build (package)
  "Function to call after building the package while updating it."
  ;; fix trailing failed installs
  (el-get-merge-properties-into-status package 'update :noerror t)
  (when (string= (el-get-read-package-status package) "required")
    (el-get-save-package-status package "installed"))
  (el-get-invalidate-autoloads package)
  (el-get-do-init package)
  (el-get-reload package)
  (run-hook-with-args 'el-get-post-update-hooks package))

(defun el-get-post-update (package)
  "Post update PACKAGE. This will get run by a sentinel."
  (let* ((sync el-get-default-process-sync)
         (type     (el-get-package-type package))
         (hooks    (el-get-method type :update-hook))
         (commands (el-get-build-commands package)))

    ;; post-update is the right place to run update-hook
    (run-hook-with-args hooks package)

    (el-get-build package commands nil sync 'el-get-post-update-build)))

(defun el-get-update-requires-reinstall (package)
  "Returns true if updating PACKAGE would require a reinstall.

This happens if the cached recipe and the current one have
different install methods."
  (let* ((source   (el-get-package-def package))
         (old-source (el-get-read-package-status-recipe package))
         (method   (el-get-package-method source))
         (old-method (el-get-package-method old-source)))
    (not (eq method old-method))))

(defun el-get-do-update (package)
  "Update "
  (el-get-error-unless-package-p package)
  (assert (el-get-package-is-installed package) nil
          "Package %s cannot be updated because it is not installed." package)
  (let* ((package (el-get-as-symbol package))
         (source   (el-get-package-def package))
         (method   (el-get-package-method source))
         (update   (el-get-method method :update))
         (url      (plist-get source :url)))
    (assert (null (remove-if 'el-get-package-is-installed
                             (el-get-dependencies package)))
            nil
            "Dependencies of package %s should already be installed before updating"
            package)
    (funcall update package url 'el-get-post-update)
    (message "el-get update %s" package)))

(defvar el-get-update-post-dependency-fun nil
  "Helper variable for updating after dependencies are installed.

This variable exists because the function that it holds is a
dynamically-generated lambda, but it needs to be able to refer to
itself.")

;;;###autoload
(defun el-get-update (package)
  "Update PACKAGE."
  (interactive
   (progn
     (el-get-read-status-file-force)
     (list (el-get-read-package-with-status "Update" "required" "installed"))))
  (el-get-error-unless-package-p package)
  (if (el-get-update-requires-reinstall package)
      (el-get-reinstall package)
    (let* ((package (el-get-as-symbol package))
           (new-dependencies (remove-if 'el-get-package-is-installed
                                        (el-get-dependencies package)))
           (source   (el-get-package-def package)))
      (if (plist-get source :checksum)
          (error "el-get: remove checksum from package %s to update it." package)
        (if new-dependencies
            ;; Package has gained new dependencies, so we need to
            ;; install them before updating.
            (progn
              ;; Prepare to install dependencies
              (setq el-get-next-packages new-dependencies)
              (add-hook 'el-get-post-init-hooks 'el-get-install-next-packages)
              (when el-get-update-post-dependency-fun
                ;; Ensure previous value is removed
                (remove-hook 'el-get-post-init-hooks
                             el-get-update-post-dependency-fun))
              ;; Set up hook to run the update only once after all
              ;; dependencies are installed, and then have the hook
              ;; disable itself.
              (setq el-get-update-post-dependency-fun
                    `(lambda (&rest ignored)
                       (when (null el-get-next-packages)
                         (remove-hook 'el-get-post-init-hooks
                                      el-get-update-post-dependency-fun)
                         (setq el-get-update-post-dependency-fun nil)
                         (el-get-do-update ',package))))
              ;; Add the hook at the *END* so that it runs after
              ;; `el-get-install-next-packages' and therefore sees the
              ;; empty dependency list.
              (add-hook 'el-get-post-init-hooks
                        el-get-update-post-dependency-fun
                        'append)
              ;; Set the update process in motion
              (el-get-install-next-packages))
          ;; update the package now
          (el-get-do-update package))))))

;;;###autoload
(defun el-get-update-all (&optional no-prompt)
  "Performs update of all installed packages."
  (interactive)
  (when (or no-prompt
            (yes-or-no-p
             "Do you really want to update all installed packages? "))
    (let ((el-get-elpa-do-refresh 'once))
      (mapc 'el-get-update (el-get-list-package-names-with-status "installed")))))

(eval-and-compile
  (unless (fboundp 'user-error)         ; new in 24.3
    (defun user-error (format &rest args)
      "Signal a pilot error, making error message by passing all args to `format'.
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency.
This is just like `error' except that `user-error's are expected to be the
result of an incorrect manipulation on the part of the user, rather than the
result of an actual problem."
      (while t
        (signal 'user-error (list (apply #'format format args)))))))

;;;###autoload
(defun el-get-update-packages-of-type (type)
  "Update all installed packages of type TYPE."
  ;; TODO Update info file about this new command
  (interactive
   (let ((types
          (mapcar #'el-get-keyword-name
                  (el-get-plist-keys el-get-methods))))
     (list (completing-read
            "Type name: " types nil t))))
  (when (not (el-get-method-defined-p type))
    (user-error "Unknown package type \"%s\"" type))
  (let ((pkgnames
         (mapcar 'car (el-get-package-types-alist
                       "installed" (intern type)))))
    (if pkgnames
        (mapcar 'el-get-update pkgnames)
      (message "No installed packages of type \"%s\"" type))))

;;;###autoload
(defun el-get-self-update ()
  "Update el-get itself.  The standard recipe takes care of reloading the code."
  (interactive)
  (let ((el-get-default-process-sync t)
        (el-get-dir
         (expand-file-name ".." (file-name-directory el-get-script))))
    ;; Delete elc files so bugs they contain won't persist.
    (mapc #'delete-file
          (nconc (directory-files (expand-file-name "el-get" el-get-dir) t "\\.elc\\'" t)
                 (directory-files (expand-file-name "el-get/methods" el-get-dir) t "\\.elc\\'" t)))
    (el-get-update "el-get")))


(defun el-get-post-remove (package)
  "Run the post-remove hooks for PACKAGE."
  (let* ((hooks   (el-get-method (el-get-package-method package) :remove-hook)))
    (run-hook-with-args hooks package)
    (run-hook-with-args 'el-get-post-remove-hooks package)))

;;;###autoload
(defun el-get-remove (package)
  "Remove any PACKAGE that is know to be installed or required."
  (interactive
   (progn
     (el-get-clear-status-cache)
     (list (el-get-read-package-with-status "Remove" "required" "installed"))))
  ;; When the user didn't select any package, just bail out
  (unless (string= package "")
    ;; If the package has a recipe saved in the status file, that will
    ;; be used. But if not, we still want to try to remove it, so we
    ;; fall back to the recipe file, and if even that doesn't provide
    ;; something, we use `el-get-rmdir' by default. This won't work for
    ;; everything, but it's better than nothing.
    (let ((fallback-source
           (or (ignore-errors (el-get-package-def package))
               (list :name package :type 'builtin))))
      (el-get-with-status-sources ()
        (let* ((source   (or (ignore-errors (el-get-package-def package))
                             fallback-source))
               ;; Put the fallback source into `el-get-sources' so that
               ;; other functions will pick it up.
               (el-get-sources (cons source el-get-sources))
               (method   (el-get-package-method source))
               (remove   (el-get-method method :remove))
               (url      (plist-get source :url)))
          ;; remove the package now
          (el-get-save-package-status package "removed")
          (el-get-remove-autoloads package)
          (funcall remove package url 'el-get-post-remove)
          (message "el-get remove %s" package))))))

;;;###autoload
(defun el-get-reinstall (package)
  "Remove PACKAGE and then install it again."
  (interactive (list (el-get-read-package-name "Reinstall")))
  (el-get-remove package)
  (el-get-install package))

(defun el-get-cleanup (packages)
  "Clean up packages installed with el-get.

In particular, keep all of the packages listed in the 'packages
argument list, and also keep all of the packages that the listed
packages depend on.  Get rid of everything else.  Note that
el-get-cleanup will not remove el-get itself, regardless of
whether or not el-get is listed in the 'packages argument list.

This is useful, for example, when we want to remove all packages not
explicitly declared in the user-init-file (.emacs)."
  (let* ((packages-to-keep (el-get-dependencies
                            (mapcar 'el-get-as-symbol
                                    (add-to-list 'packages 'el-get))))
         (packages-to-remove (set-difference
                              (mapcar 'el-get-as-symbol
                                      (el-get-list-package-names-with-status
                                       "installed")) packages-to-keep)))
    (mapc 'el-get-remove packages-to-remove)))



;;;###autoload
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
        (emacs-lisp-mode)
        (el-get-recipe-pprint source)))))

;;;###autoload
(defun el-get-make-recipes (&optional dir)
  "Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe."
  (interactive "Dsave recipes in directory: ")
  (let* ((all (el-get-read-all-recipe-names))
         (new (loop for r in el-get-sources
                    when (and (not (symbolp r))
                              (not (member (el-get-source-name r) all)))
                    collect r)))
    (dolist (r new)
      (message "el-get: preparing recipe file for %s" (el-get-source-name r))
      (el-get-write-recipe r dir)))
  (dired dir))

;;;###autoload
(defun el-get-checksum (package)
  "Compute the checksum of the given package, and put it in the kill-ring"
  (interactive
   (list (el-get-read-package-with-status "Checksum" "installed")))
  (el-get-with-status-sources ()
    (let* ((type             (el-get-package-type package))
           (checksum         (plist-get (el-get-package-def package) :checksum))
           (compute-checksum (el-get-method type :compute-checksum)))
      (when (and checksum (not compute-checksum))
        (error "package method %s does not support checksums" type))
      (when compute-checksum
        (let ((checksum (funcall compute-checksum package)))
          (message "Checksum for package %s is: %s. It has been copied to the kill-ring."
                   package checksum)
          (kill-new checksum)
          checksum)))))

;;;###autoload
(defun el-get-self-checksum ()
  "Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring."
  (interactive)
  (el-get-checksum 'el-get))


;;
;; User Interface, Non Interactive part
;;
(defun el-get-init-and-install (&optional packages)
  "Install \"required\" packages, init \"installed\" packages.

When PACKAGES is non-nil, only process entries from this list.
Those packages from the list we don't know the status of are
considered \"required\"."
  (let* ((p-s-alist   (el-get-read-status-file))
         (required    (el-get-filter-package-alist-with-status p-s-alist "required"))
         (installed   (el-get-filter-package-alist-with-status p-s-alist "installed"))
         (to-init     (if packages
                          (loop for p in packages
                                when (member (el-get-as-string p) installed)
                                collect p)
                        (mapcar 'el-get-as-symbol installed)))
         (init-deps   (el-get-dependencies to-init))
         (req-inits   (loop for p in init-deps
                                unless (member (el-get-as-string p) installed)
                                collect p))
         (to-install  (if packages
                          (loop for p in packages
                                unless (member p init-deps)
                                collect p)
                        (mapcar 'el-get-as-symbol required)))
         (install-deps (el-get-dependencies to-install))
         done)
    (when req-inits       ; we can't init a pkg unless it's installed!
      (setq install-deps (append req-inits install-deps))
      (setq init-deps (set-difference init-deps req-inits)))

    (el-get-verbose-message "el-get-init-and-install: install %S" install-deps)
    (el-get-verbose-message "el-get-init-and-install: init %S" init-deps)

    (loop for p in install-deps
          when (el-get-with-errors-as-warnings (format "while installing %s: " p)
                 (el-get-do-install p))
          collect p into done)
    (loop for p in init-deps
          when (el-get-with-errors-as-warnings (format "while initializing %s: " p)
                 (el-get-do-init p))
          collect p into done)
    done))

;;;###autoload
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

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered."
  ;; Autoloads path are relative to el-get-dir, so add it to load-path
  (add-to-list 'load-path (file-name-as-directory el-get-dir))

  (let* ((packages
          ;; (el-get 'sync 'a 'b my-package-list)
          (loop for p in packages when (listp p) append p else collect p))
         (el-get-default-process-sync sync))

    ;; load autoloads before package init so :after blocks can use the
    ;; autoloaded functions.
    (unless el-get-is-lazy ; :after blocks aren't run til later when lazy
      (el-get-eval-autoloads))

    ;; keep the result of `el-get-init-and-install' to return it even in the
    ;; 'wait case
    (prog1
        (el-get-init-and-install (mapcar 'el-get-as-symbol packages))

      ;; now is a good time to care about autoloads
      (el-get-eval-autoloads))))

(provide 'el-get)

;;; el-get.el ends here
