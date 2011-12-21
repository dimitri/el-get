;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Version: 4.0
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
;;  4.0 - WIP - To infinity, and beyond!
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

;; first some essential variables, used in other parts of the code.
(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defconst el-get-version "4.0.6" "el-get version number")

(defconst el-get-script (or load-file-name buffer-file-name))

(defcustom el-get-dir "~/.emacs.d/el-get/"
  "Path where to install the packages."
  :group 'el-get
  :type 'directory)

(defcustom el-get-status-file
  (concat (file-name-as-directory el-get-dir) ".status.el")
  "Define where to store and read the package statuses")

(defvar el-get-autoload-file
  (concat (file-name-as-directory el-get-dir) ".loaddefs.el")
  "Where generated autoloads are saved")

(defvar el-get-emacs (concat invocation-directory invocation-name)
  "Where to find the currently running emacs, a facility for :build commands")

;;
;; Now load the rest of the el-get code
;;
(require 'el-get-core)			; core facilities used everywhere
(require 'el-get-custom)		; user tweaks and `el-get-sources'
(require 'el-get-methods)		; support for `el-get-methods', backends
(require 'el-get-recipes)		; support for dealing with recipes
(require 'el-get-status)		; support for dealing with status
(require 'el-get-build)			; building packages
(require 'el-get-byte-compile)		; byte compiling in a subprocess
(require 'el-get-dependencies)		; topological-sort of package dep graph
(require 'el-get-notify)		; notification support (dbus, growl...)
(require 'el-get-list-packages)		; menu and `el-get-describe' facilities
(require 'el-get-autoloads)		; manages updating el-get's loaddefs.el

;;
;; And then define some more code-level customs.  They stay here so that
;; it's easier for elisp programmers to find them and know about them.  If
;; that is too lame of an excuse, let's move them to el-get-custom.el.
;;
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

(defun el-get-funcall (func fname package)
  "`funcal' FUNC for PACKAGE and report about FNAME when `el-get-verbose'"
  (when (and func (functionp func))
      (el-get-verbose-message "el-get: Calling :%s function for package %s"
			      fname package)
      ;; don't forget to make some variables available
      (let ((pdir (el-get-package-directory package)))
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

        (el-get-funcall postinit "post-init" package)

        ;; now handle the user configs and :after functions
        (if (or lazy el-get-is-lazy)
            (let ((lazy-form
		   `(progn (el-get-load-package-user-init-file ',package)
			   ,(when after (list 'funcall after)))))
              (eval-after-load library lazy-form))

          ;; el-get is not lazy here
	  (el-get-load-package-user-init-file package)
          (el-get-funcall after "after" package))

        ;; and call the global init hooks
        (run-hook-with-args 'el-get-post-init-hooks package)

        ;; return the package
        package)
    (debug error
     (el-get-installation-failed package err))))


(defun el-get-install (package)
  "Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol."
  (interactive (list (el-get-read-package-name "Install")))
  (let ((packages  (el-get-dependencies (el-get-as-symbol package))))
    (when (cdr packages)
      ;; tweak el-get-post-install-hooks to install remaining packages
      ;; once the first is installed
      (el-get-verbose-message "el-get-install %s: %S" package packages)
      (setq el-get-next-packages (cdr packages))
      (add-hook 'el-get-post-install-hooks 'el-get-install-next-packages))

    (let ((package (car packages)))
      (if (not (el-get-package-is-installed package))
	  (el-get-do-install package)
	;; if package is already installed, skip to the next
	(message "el-get: `%s' package is already installed" package)
	(el-get-init package)
	(el-get-install-next-packages package)))))

(defun el-get-install-next-packages (current-package)
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
  (el-get-invalidate-autoloads package)	; that will also update them
  (el-get-init package))

(defun el-get-post-install (package)
  "Post install PACKAGE. This will get run by a sentinel."
  (let* ((sync             el-get-default-process-sync)
	 (type             (el-get-package-type package))
	 (hooks            (el-get-method type :install-hook))
	 (commands         (el-get-build-commands package))
	 (checksum         (plist-get (el-get-package-def package) :checksum))
	 (compute-checksum (el-get-method type :compute-checksum)))

    ;; check the checksum of the package here, as early as possible
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
				  package computed))))

    ;; post-install is the right place to run install-hook
    (run-hook-with-args hooks package)

    ;; el-get-post-build will care about autoloads and initializing the
    ;; package, and will change the status to "installed"
    (el-get-build package commands nil sync 'el-get-post-install-build))
  (run-hook-with-args 'el-get-post-install-hooks package))

(defun el-get-do-install (package)
  "Install any PACKAGE for which you have a recipe."
  (el-get-error-unless-package-p package)
  (if (string= (el-get-package-status package) "installed")
      (el-get-init package)
    (let* ((status   (el-get-read-package-status package))
	   (source   (el-get-package-def package))
	   (method   (el-get-package-method source))
	   (install  (el-get-method method :install))
	   (url      (plist-get source :url))
           (pdir     (el-get-package-directory package)))

      (cond ((string= "installed" status)
             (error "Package %s is already installed." package))
            ((string= "required" status)
             (message "Package %s failed to install, removing it first." package)
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

(defun el-get-reload (package)
  "Reload PACKAGE."
  (interactive
   (list (el-get-read-package-with-status "Update" "installed")))
  (el-get-verbose-message "el-get-reload: %s" package)
  (let* ((all-features features)
         (package-features (el-get-package-features package))
         (package-files (el-get-package-files package))
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
                do (load file 'noerror))
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
          (setq features (append missing-features features)))))))


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
                    (el-get-reload package)
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
    (if (plist-get source :checksum)
	(error "el-get: remove checksum from package %s to update it." package)
      (funcall update package url 'el-get-post-update)
      (message "el-get update %s" package))))

;;;###autoload
(defun el-get-update-all ()
  "Performs update of all installed packages."
  (interactive)
  (mapc 'el-get-update (el-get-list-package-names-with-status "installed")))

;;;###autoload
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
	(insert (prin1-to-string source))))))

;;;###autoload
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

;;;###autoload
(defun el-get-checksum (package)
  "Compute the checksum of the given package, and put it in the kill-ring"
  (interactive
   (list (el-get-read-package-with-status "Checksum" "installed")))
  (let* ((type             (el-get-package-type package))
	 (checksum         (plist-get (el-get-package-def package) :checksum))
	 (compute-checksum (el-get-method type :compute-checksum)))
    (when (and checksum (not compute-checksum))
      (error "package method %s does not support checksums" type))
    (when compute-checksum
      (let ((checksum (funcall compute-checksum package)))
	(message "Checksum for package %s is: %s" package checksum)
	(kill-new checksum)))))


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
				collect p)
			(mapcar 'el-get-as-symbol installed)))
	 (init-deps   (el-get-dependencies to-init))
	 (to-install  (if packages
			  (loop for p in packages
				unless (member p init-deps)
				collect p)
			(mapcar 'el-get-as-symbol required)))
	 (install-deps (el-get-dependencies to-install))
	 done)
    (el-get-verbose-message "el-get-init-and-install: install %S" install-deps)
    (el-get-verbose-message "el-get-init-and-install: init %S" init-deps)

    (loop for p in install-deps do (el-get-do-install p) collect p into done)
    (loop for p in init-deps    do (el-get-init p)       collect p into done)
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
  (unless (file-exists-p el-get-autoload-file) (el-get-invalidate-autoloads))

  ;; Autoloads path are relative to el-get-dir, so add it to load-path
  (add-to-list 'load-path (file-name-as-directory el-get-dir))

  (let* ((packages
	  ;; (el-get 'sync 'a 'b my-package-list)
	  (loop for p in packages when (listp p) append p else collect p))
	 (p-status    (el-get-read-all-packages-status))
         (total       (length packages))
         (installed   (el-get-count-packages-with-status packages "installed"))
         (progress (and (eq sync 'wait)
                        (make-progress-reporter
			 "Waiting for `el-get' to complete... "
			 0 (- total installed) 0)))
         (el-get-default-process-sync sync))

    ;; keep the result of `el-get-init-and-install' to return it even in the
    ;; 'wait case
    (prog1
	(el-get-init-and-install (mapcar 'el-get-as-symbol packages))

      ;; el-get-install is async, that's now ongoing.
      (when progress
        (while (> (- total installed) 0)
          (sleep-for 0.2)
          ;; don't forget to account for installation failure
          (setq installed (el-get-count-packages-with-status packages "installed" "required"))
          (progress-reporter-update progress (- total installed)))
        (progress-reporter-done progress))

      ;; now is a good time to care about autoloads
      (el-get-eval-autoloads))))

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
