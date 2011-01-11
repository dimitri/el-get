;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 1.2~dev
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs svn darcs hg
;;           apt-get fink pacman http http-tar emacswiki
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.asciidoc file from the same distribution
;;
;; Changelog
;;
;;  1.2 - WIP - Still growing
;;
;;   - Add support for autoloads, per Dave Abrahams
;;   - fix 'wait support for http (using sync retrieval)
;;   - code cleanup per Dave Abrahams, lots of it
;;   - add function M-x el-get-update-all
;;   - Implement M-x el-get-make-recipes
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
;;

(require 'dired)
(require 'package nil t) ; that's ELPA, but you can use el-get to install it
(require 'cl)            ; needed for `remove-duplicates'
(require 'bytecomp)
(require 'autoload)

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defconst el-get-version "1.1~dev" "el-get version number")

(defcustom el-get-post-init-hooks nil
  "Hooks to run after a package init.
It will get called with the package as first argument."
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-install-hooks nil
  "Hooks to run after installing a package.
It will get called with the package as first argument."
  :group 'el-get
  :type 'hook)

(defcustom el-get-post-update-hooks nil
  "Hooks to run after updating a package.
It will get called with the package as first argument."
  :group 'el-get
  :type 'hook)

(defcustom el-get-byte-compile t
  "Whether or not to byte-compile packages. Can be used to
disable byte-compilation globally."
  :group 'el-get
  :type 'boolean)

(defvar el-get-git-clone-hook        nil "Hook run after git clone.")
(defvar el-get-git-svn-clone-hook    nil "Hook run after git svn clone.")
(defvar el-get-bzr-branch-hook       nil "Hook run after bzr branch.")
(defvar el-get-cvs-checkout-hook     nil "Hook run after cvs checkout.")
(defvar el-get-svn-checkout-hook     nil "Hook run after svn checkout.")
(defvar el-get-darcs-get-hook        nil "Hook run after darcs get.")
(defvar el-get-apt-get-install-hook  nil "Hook run after apt-get install.")
(defvar el-get-apt-get-remove-hook   nil "Hook run after apt-get remove.")
(defvar el-get-fink-install-hook     nil "Hook run after fink install.")
(defvar el-get-fink-remove-hook      nil "Hook run after fink remove.")
(defvar el-get-elpa-install-hook     nil "Hook run after ELPA package install.")
(defvar el-get-elpa-remove-hook      nil "Hook run after ELPA package remove.")
(defvar el-get-http-install-hook     nil "Hook run after http retrieve.")
(defvar el-get-http-tar-install-hook nil "Hook run after http-tar package install.")
(defvar el-get-pacman-install-hook   nil "Hook run after pacman install.")
(defvar el-get-pacman-remove-hook    nil "Hook run after pacman remove.")
(defvar el-get-hg-clone-hook         nil "Hook run after hg clone.")

(defcustom el-get-methods
  '(:git     (:install el-get-git-clone
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

(defvar el-get-dir "~/.emacs.d/el-get/"
  "Define where to fetch the packages.")

(defvar el-get-recipe-path '("~/.emacs.d/el-get/el-get/recipes")
  "Define where to look for the recipes")

(defvar el-get-status-file
  (concat (file-name-as-directory el-get-dir) ".status.el")
  "Define where to store and read the package statuses")

(defvar el-get-autoload-file
  (concat (file-name-as-directory el-get-dir) ".loaddefs.el")
  "Where generated autoloads are saved")

(defvar el-get-outdated-autoloads nil
  "List of package names whose autoloads are outdated")

(defvar el-get-emacs (concat invocation-directory invocation-name)
  "Where to find the currently running emacs, a facility for :build commands")

(defvar el-get-apt-get (executable-find "apt-get")
  "The apt-get executable.")

(defvar el-get-apt-get-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended.")

(defvar el-get-fink (executable-find "fink")
  "The fink executable.")

(defvar el-get-svn (executable-find "svn")
  "The svn executable.")

(defvar el-get-fink-base "/sw/share/doc"
  "Where to link the el-get symlink to, /<package> will get appended.")

(defvar el-get-emacswiki-base-url
  "http://www.emacswiki.org/emacs/download/%s.el"
  "The base URL where to fetch :emacswiki packages")

(defvar el-get-pacman-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended.")

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
				(executable-find "install-info")))

;; we support notifications on darwin too, thanks to growlnotify
(defvar el-get-growl-notify "/usr/local/bin/growlnotify")

(defvar el-get-sources nil
  "List of sources for packages.

Each source entry is either a symbol, in which case the first
recipe found in `el-get-recipe-path' directories named after the
symbol with a \".el\" extension will get used, or a PLIST where
the following properties are supported.

If your property list is missing the :type property, then it's
merged with the recipe one, so that you can override any
definition provided by `el-get' recipes locally.

:name

    The name of the package. It can be different from the name of
    the directory where the package is stored (after a `git
    clone' for example, in which case a symlink will be created.

:pkgname

    The name of the package for the underlying package management
    system (`apt-get', `fink' or `pacman'), which can be
    different from the Emacs package name.

:type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git' and `http'. You can easily support
    your own types here, see the variable `el-get-methods'.

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

    This should be a list of directories you want `el-get' to add
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

:before

    A pre-init function to run once before `el-get-init' calls
    `load' and `require'.

:after

    A function to run once `el-get' is done with `el-get-init',
    can be a lambda.

:localname

    Currently only used by both `http' and `ftp' supports, allows
    to specify the target name of the downloaded file.

    This option is useful if the package should be retrieved using
    a presentation interface (such as as web SCM tool).

    For example, destination should be set to \"package.el\" if
    the package url has the following scheme:

   \"http://www.example.com/show-as-text?file=path/package.el\"

")


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
  (let* ((method  (intern-soft (concat ":" (format "%s" method-name))))
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
;; call-process-list utility, to do same as bash && feature
;;
(defun el-get-start-process-list-sentinel (proc change)
  "When proc has exited and was successful, chain next command."
  (when (eq (process-status proc) 'exit)
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
	  (funcall final-f package))))))

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

Any other property will get put into the process object.
"
  (when commands
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
	   (default-directory (if cdir
				  (file-name-as-directory
				   (expand-file-name cdir))
				default-directory)))
      (if sync
          (let* ((startf (if shell #'call-process-shell-command #'call-process))
                 (dummy  (message "el-get is waiting for %S to complete" cname))
                 (status (apply startf program nil cbuf t args))
                 (message (plist-get c :message))
                 (errorm  (plist-get c :error))
                 (next    (cdr commands)))
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
          (set-process-sentinel proc 'el-get-start-process-list-sentinel)
          (when filter (set-process-filter proc filter))))))
  ;; no commands, still run the final-func
  (unless commands
    (when (functionp final-func)
      (funcall final-func package))))


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
  (let* ((git-executable (el-get-git-executable))
	 (pdir (el-get-package-directory package))
	 (name (format "*git clone %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ( "--no-pager" "clone" ,url ,package)
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
  (let* ((git-executable (el-get-git-executable))
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
;; git-svn support
;;
(defun el-get-git-svn-clone (package url post-install-fun)
  "Clone the given svn PACKAGE following the URL using git."
  (let ((git-executable (el-get-git-executable))
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
  (let ((git-executable (el-get-git-executable))
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
  (let* ((bzr-executable "bzr")
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
  (let* ((bzr-executable "bzr")
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
  (let* ((svn-executable el-get-svn)
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
  (let* ((svn-executable el-get-svn)
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
  (let* ((cvs-executable (executable-find "cvs"))
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
  (let* ((cvs-executable (executable-find "cvs"))
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
(defun el-get-darcs-executable ()
  "Return darcs executable to use, or signal an error when not
found."
  (let ((darcs-executable (executable-find "darcs")))
    (unless (and darcs-executable (file-executable-p darcs-executable))
      (error "The  `darcs' binary can not be found in your PATH"))
    darcs-executable))

(defun el-get-darcs-get (package url post-install-fun)
  "Get a given PACKAGE following the URL using darcs."
  (let* ((darcs-executable (el-get-darcs-executable))
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
  (let* ((darcs-executable (el-get-darcs-executable))
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
		      :args ( "pull" )
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
	 (method  (plist-get (el-get-package-def package) :type))
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
      (message (concat "cd " el-get-dir " && rm -f " package))
      (shell-command
       (concat "cd " el-get-dir " && rm -f " package)))))


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

(defun el-get-elpa-symlink-package (package)
  "ln -s ../elpa/<package> ~/.emacs.d/el-get/<package>"
  (let ((elpa-dir (file-relative-name
		   (el-get-elpa-package-directory package) el-get-dir)))
    (unless (el-get-package-exists-p package)
      (message "%s"
       (shell-command
	(concat "cd " el-get-dir
		" && ln -s \"" elpa-dir "\" \"" package "\""))))))

(defun el-get-elpa-install (package url post-install-fun)
  "Ask elpa to install given PACKAGE."
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      (package-install (intern-soft package)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package))
  (funcall post-install-fun package))

(defun el-get-elpa-update (package url post-update-fun)
  "Ask elpa to update given PACKAGE."
  (el-get-elpa-remove package url nil)
  (package-install (intern-soft package))
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
(defun el-get-http-retrieve-callback (status package post-install-fun &optional dest sources)
  "Callback function for `url-retrieve', store the emacs lisp file for the package."
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (or dest (concat (file-name-as-directory pdir) package ".el")))
	 (part   (concat dest ".part"))
	 (el-get-sources (if sources sources el-get-sources))
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
		     (file-name-nondirectory url)))
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


;;
;; http-tar support (archive)
;;
(defun el-get-http-tar-cleanup-extract-hook (package)
  "Cleanup after tar xzf: if there's only one subdir, move all
the files up."
  (let* ((pdir    (el-get-package-directory package))
	 (url     (plist-get (el-get-package-def package) :url))
	 (tarfile (file-name-nondirectory url))
	 (files   (directory-files pdir nil "[^.]$")))
    ;; if there's only one directory, move its content up and get rid of it
    (message "%S" (remove tarfile files))
    (when (null (cdr (remove tarfile files)))
      (let ((move  (format "cd %s && mv \"%s\"/* ." pdir (car files)))
	    (rmdir (format "cd %s && rmdir \"%s\""   pdir (car files))))
	;; (message "%s: %s" package move)
	;; (message "%s: %s" package rmdir)
	(shell-command move)
	(shell-command rmdir)))))

(defun el-get-http-tar-install (package url post-install-fun)
  "Dowload a tar archive package over HTTP."
  (let* ((source  (el-get-package-def package))
	 (options (plist-get source :options))
	 (pdir    (el-get-package-directory package))
	 (tarfile (file-name-nondirectory url))
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
  (let* ((hg-executable "hg")
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
  (let* ((hg-executable "hg")
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
  (let ((pdir (el-get-package-directory package)))
    (if (file-exists-p pdir)
	(dired-delete-file pdir 'always)
      (message "el-get could not find package directory \"%s\"" pdir))
    (funcall post-remove-fun package)))

(defun el-get-set-info-path (package infodir-rel)
  (require 'info)
  (info-initialize)
  (el-get-add-path-to-list package 'Info-directory-list infodir-rel))

(defun el-get-install-or-init-info (package build-or-init)
  "Call `el-get-install-info' to create the necessary \"dir\"
  file when build-or-init is 'build, or `el-get-set-info-path'
  when build-or-init is 'init "
  (let* ((source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (infodir  (plist-get source :info))
	 (pdir     (el-get-package-directory package)))

    ;; apt-get, pacman and ELPA will take care of Info-directory-list
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
			   (concat infofile ".info")))) infodir-rel t)))

	  (t
	   (error
	    "el-get-install-or-init-info: %s not supported" build-or-init)))))))

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

(defun el-get-build (package commands &optional subdir sync post-build-fun)
  "Run each command from the package directory."
  (let* ((pdir   (el-get-package-directory package))
	 (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir))
	 (buf    (format "*el-get-build: %s*" package))
	 (default-directory wdir))

    ;; first build the Info dir
    (el-get-install-or-init-info package 'build)

    (if sync
	(progn
	  (dolist (c commands)
            (let ((cmd
                   (if (stringp c) c
                     (mapconcat 'shell-quote-argument c " "))))
              (message "%S" (shell-command-to-string cmd))))
	  (when (and post-build-fun (functionp post-build-fun))
	    (funcall post-build-fun)))

      ;; async
      (let ((process-list
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
					 :program ,program
					 :args (,@args)
					 :message ,(format "el-get-build %s: %s ok." package c)
					 :error ,(format
						  "el-get could not build %s [%s]" package c))))
		     commands)))
	(el-get-start-process-list package process-list post-build-fun)))))


;;
;; recipes
;;
(defun el-get-read-recipe-file (filename)
  "Read given filename and return its content (a valid form is expected)"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (read (current-buffer))))

(defun el-get-read-recipe (package)
  "Return the source definition for PACKAGE, from the recipes."
  (loop for dir in el-get-recipe-path
	for recipe = (concat (file-name-as-directory dir) package ".el")
	if (file-exists-p recipe)
	return (el-get-read-recipe-file recipe)))

(defun el-get-read-all-recipes (&optional merge)
  "Return the list of all the recipes, formatted like `el-get-sources'.

Only consider any given recipe only once even if present in
multiple dirs from `el-get-recipe-path'. The first recipe found
is the one considered.

When MERGE is non-nil, the recipes from `el-get-recipe-path' will
get merged to `el-get-sources'."
  (let ((packages (when merge (mapcar 'el-get-source-name el-get-sources))))
    (append
     (when merge el-get-sources)
     (loop for dir in el-get-recipe-path
	   nconc (loop for recipe in (directory-files dir nil "\.el$")
		       for filename = (concat (file-name-as-directory dir) recipe)
		       and package = (file-name-sans-extension (file-name-nondirectory recipe))
		       unless (member package packages)
		       do (push package packages)
		       and collect (el-get-read-recipe-file filename))))))

(defun el-get-source-name (source)
  "Return the package name (stringp) given an `el-get-sources'
entry."
  (if (symbolp source) (symbol-name source)
    (format "%s" (plist-get source :name))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for PACKAGE."
  (let ((source (loop for src in el-get-sources
		      when (string= package (el-get-source-name src))
		      return src)))

    (cond ((symbolp source)
	   ;; we did find only its name, load its definition in the recipes
	   (el-get-read-recipe package))

	  ((null (plist-get source :type))
	   ;; we got a list with no :type, that's an override plist
	   (loop with def = (el-get-read-recipe package)
		 for (prop override) on source by 'cddr
		 do (plist-put def prop override)
		 finally return def))

	  ;; none of the previous, must be a full definition
	  ;; definition
	  (t source))))


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

(defun el-get-count-package-with-status (&rest status)
  "Return how many packages are currently in given status"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	if (member s status) sum 1))

(defun el-get-package-status (package &optional package-status-plist)
  "Return current status of package from given list"
  (let ((status-plist (or package-status-plist (el-get-read-all-packages-status))))
    (plist-get status-plist (el-get-package-symbol package))))

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

(defun el-get-package-name-list (&optional merge-recipes)
  "Return package a list of all package names from
`el-get-sources'."
  (let* ((el-get-sources    (if merge-recipes (el-get-read-all-recipes 'merge)
			      el-get-sources))
	 (package-name-list (mapcar 'el-get-source-name el-get-sources))
	 (duplicates        (el-get-duplicates package-name-list)))
    (when duplicates
      (error "Please remove duplicates in `el-get-sources': %S." duplicates))
    package-name-list))

(defun el-get-package-p (package)
  "Check that PACKAGE is actually a valid package according to
`el-get-sources'."
  ;; don't check for duplicates in this function
  (member package (mapcar 'el-get-source-name el-get-sources)))

(defun el-get-error-unless-package-p (package)
  "Raise en error if PACKAGE is not a valid package according to
`el-get-package-p'."
  (unless (el-get-package-p package)
    (error "el-get: can not find package name `%s' in `el-get-sources'" package))
  ;; check for recipe too
  (let ((recipe (el-get-package-def package)))
    (unless recipe
      (error "el-get: package `%s' has no recipe" package))
    (unless (plist-member recipe :type)
      (error "el-get: package `%s' has incomplete recipe (no :type)" package))))

(defun el-get-read-package-name (action &optional merge-recipes)
  "Ask user for a package name in minibuffer, with completion."
  (completing-read (format "%s package: " action)
                   (el-get-package-name-list merge-recipes) nil t))

(defun el-get-byte-compile-file (el)
  "byte-compile the file EL if there's no .elc or the source is newer"
  (let* ((elc (concat (file-name-sans-extension el) ".elc")))
    (when (or (not (file-exists-p elc))
	      (file-newer-than-file-p el elc))
      (byte-compile-file el))))

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
  (load (file-name-sans-extension file)))

(defun el-get-eval-autoloads ()
  "Evaluate the autoloads from the autoload file."
  (message "el-get: evaluating autoload file")
  (el-get-load-fast el-get-autoload-file))

(defun el-get-update-autoloads ()
  "Regenerate, compile, and load any outdated packages' autoloads.

This function will run from `post-command-hook', and usually
shouldn't be invoked directly."

  (message "el-get: updating outdated autoloads")
  (setq el-get-autoload-timer nil) ;; Allow a new update to be primed

  (let ((outdated el-get-outdated-autoloads)
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

    (message "el-get: byte-compiling autoload file")
    (when el-get-byte-compile
      (el-get-byte-compile-file el-get-autoload-file))

    (el-get-eval-autoloads)))

(defconst el-get-load-suffix-regexp
  (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "\\'"))

(defun el-get-remove-autoloads (package)
  "Remove from `el-get-autoload-file' any autoloads associated
with the named PACKAGE"
  (with-temp-buffer ;; empty buffer to trick `autoload-find-destination'
    (let ((generated-autoload-file el-get-autoload-file)
          (autoload-modified-buffers (list (current-buffer))))
      (dolist (dir (el-get-load-path package))
        (when (file-directory-p dir)
          (dolist (f (directory-files dir t el-get-load-suffix-regexp))
            ;; this will clear out any autoloads associated with the file
	    ;; `autoload-find-destination' signature has changed in emacs24.
	    (if (> emacs-major-version 23)
		(autoload-find-destination f (autoload-file-load-name f))
	      (autoload-find-destination f)))))))
  (el-get-save-and-kill el-get-autoload-file))

(defvar el-get-autoload-timer nil
  "Where the currently primed autoload timer (if any) is stored")

(defun el-get-invalidate-autoloads ( &optional package )
  "Mark the named PACKAGE as needing new autoloads.  If PACKAGE
is nil, marks all installed packages as needing new autoloads."

  ;; Trigger autoload recomputation unless it's already been done
  (unless el-get-autoload-timer
    (setq el-get-autoload-timer
          (run-with-idle-timer 0 nil 'el-get-update-autoloads)))

  ;; Save the package names for later
  (mapc (lambda (p) (add-to-list 'el-get-outdated-autoloads p))
        (if package (list package)
	  (mapcar 'el-get-source-name el-get-sources)))

  ;; If we're invalidating everything, try to start from a clean slate
  (unless package
    (ignore-errors
      (delete-file el-get-autoload-file)
      (delete-file
       (concat (file-name-sans-extension el-get-autoload-file) ".elc")))))

(defun el-get-init (package &optional noerror)
  "Make the named PACKAGE available for use.

Add PACKAGE's directory (or `:load-path' if specified) to the
`load-path', add any its `:info' directory to
`Info-directory-list', and `require' its `:features'.  Will be
called by `el-get' (usually at startup) for each package in
`el-get-sources'.

Optional parameter NOERROR, if non-nil, suppresses errors in case
package is not listed in `el-get-sources'"
  (interactive (list (el-get-read-package-name "Init")))
  (unless noerror (el-get-error-unless-package-p package))
  (let* ((source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (loads    (plist-get source :load))
	 (feats    (plist-get source :features))
	 (el-path  (el-get-load-path package))
	 (compile  (plist-get source :compile))
	 (nocomp   (and (plist-member source :compile) (not compile)))
	 (after    (plist-get source :after))
	 (before   (plist-get source :before))
	 (pdir     (el-get-package-directory package)))

    ;; append entries to load-path and Info-directory-list
    (unless (member method '(elpa apt-get fink pacman))
      ;; append entries to load-path
      (mapc (lambda (path)
	      (el-get-add-path-to-list package 'load-path path))
	    (if (stringp el-path) (list el-path) el-path))
      ;;  and Info-directory-list
      (el-get-install-or-init-info package 'init))

    (when el-get-byte-compile
      ;; byte-compile either :compile entries or anything in load-path
      (let ((byte-compile-warnings nil))
        (if compile
	    ;; only byte-compile what's in the :compile property of the recipe
            (dolist (path (if (listp compile) compile (list compile)))
              (let ((fp (concat pdir path)))
                ;; we accept directories, files and file name regexp
                (cond ((file-directory-p fp) (byte-recompile-directory fp 0))
                      ((file-exists-p fp)    (el-get-byte-compile-file fp))
                      (t ; regexp case
                       (dolist (file (directory-files pdir nil path))
                         (el-get-byte-compile-file (concat pdir file)))))))
          ;; Compile that directory, unless users asked not to (:compile nil)
	  ;; or unless we have build instructions (then they should care)
          ;; or unless we have installed pre-compiled package
          (unless (or nocomp
                      (el-get-build-commands package)
                      (member method '(apt-get fink pacman)))
            (dolist (dir el-path)
              (byte-recompile-directory dir 0))))))

    ;; call the "before" user function
    (when (and before (functionp before))
      (message "el-get: Calling :before function for package %s" package)
      (funcall before))

    ;; loads
    (when loads
      (mapc (lambda (file)
	      (let ((pfile (concat pdir file)))
		(unless (file-exists-p pfile)
		  (error "el-get could not find file '%s'" pfile))
		(message "el-get: load '%s'" pfile)
		(el-get-load-fast pfile)))
	    (if (stringp loads) (list loads) loads)))

    ;; features, only ELPA will handle them on its own
    (unless (eq method 'elpa)
      ;; if a feature is provided, require it now
      (when feats
	(mapc (lambda (feat)
		(let ((feature (if (stringp feat) (intern feat) feat)))
		  (message "require '%s" (require feature))))
	      (cond ((symbolp feats) (list feats))
		    ((stringp feats) (list (intern feats)))
		    (t feats)))))

    ;; call the "after" user function
    (when (and after (functionp after))
      (message "el-get: Calling :after function for package %s" package)
      (funcall after))

    ;; and call the global init hooks
    (run-hook-with-args 'el-get-post-init-hooks package)

    ;; return the package
    package))

(defun el-get-post-install (package &optional noerror)
  "Post install PACKAGE. This will get run by a sentinel.

Optional parameter NOERROR, if non-nil, suppresses errors in case
package is not listed in `el-get-sources'"
  (let* ((source   (el-get-package-def package))
	 (hooks    (el-get-method (plist-get source :type) :install-hook))
	 (commands (el-get-build-commands package)))

    ;; post-install is the right place to run install-hook
    (run-hook-with-args hooks package)

    (let ((wrap-up `(lambda (package)
                     (el-get-invalidate-autoloads package)
                     (el-get-init package ,noerror)
                     (el-get-save-package-status package "installed"))))
      (el-get-build package commands nil nil wrap-up)))

  (run-hook-with-args 'el-get-post-install-hooks package))

(defun el-get-install (package)
  "Install PACKAGE.

When using C-u, `el-get-install' will allow for installing any
package you have a recipe for, instead of only proposing packages
from `el-get-sources'.
"
  (interactive (list (el-get-read-package-name "Install" current-prefix-arg)))
  ;; use dynamic binding to pretend package is part of `el-get-sources'
  ;; without having to edit the user setup --- that's what C-u is for.
  (let ((el-get-sources (if current-prefix-arg
			    (el-get-read-all-recipes 'merge)
			  el-get-sources)))
    (el-get-error-unless-package-p package)

    (let* ((status   (el-get-read-package-status package))
	   (source   (el-get-package-def package))
	   (method   (plist-get source :type))
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

      (el-get-invalidate-autoloads package)

      ;; and install the package now, *then* message about it
      (funcall install package url 'el-get-post-install)
      (message "el-get install %s" package))))

(defun el-get-post-update (package)
  "Post update PACKAGE. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (commands (el-get-build-commands package)))
    (el-get-build package commands nil nil
		  (lambda (package)
		    (el-get-init package)
		    ;; fix trailing failed installs
		    (when (string= (el-get-read-package-status package) "required")
		      (el-get-save-package-status package "installed"))
                    (run-hook-with-args 'el-get-post-update-hooks package)))))

(defun el-get-update (package)
  "Update PACKAGE."
  (interactive (list (el-get-read-package-name "Update")))
  (el-get-error-unless-package-p package)
  (let* ((source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (update   (el-get-method method :update))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))
    ;; update the package now
    (funcall update package url 'el-get-post-update)
    (message "el-get update %s" package)))

(defun el-get-update-all ()
  (interactive)
  "Performs update of all installed packages (specified in el-get-sources)"
  (mapc 'el-get-update (el-get-package-name-list)))

(defun el-get-post-remove (package)
  "Run the post-remove hooks for PACKAGE."
  (let* ((source  (el-get-package-def package))
	 (hooks   (el-get-method (plist-get source :type) :remove-hook)))
    (run-hook-with-args hooks package)))

(defun el-get-remove (package)
  "Remove PACKAGE.

When using C-u, `el-get-remove' will allow for removing any
package you have a recipe for, instead of only proposing packages
from `el-get-sources'."
  (interactive (list (el-get-read-package-name "Remove" current-prefix-arg)))
  ;; see comment in el-get-install
  (let ((el-get-sources (if current-prefix-arg
			    (el-get-read-all-recipes 'merge)
			  el-get-sources)))
    (el-get-error-unless-package-p package)
    (let* ((source   (el-get-package-def package))
	   (method   (plist-get source :type))
	   (remove   (el-get-method method :remove))
	   (url      (plist-get source :url)))
      ;; remove the package now
      (el-get-remove-autoloads package)
      (funcall remove package url 'el-get-post-remove)
      (el-get-save-package-status package "removed")
      (message "el-get remove %s" package))))

(defun el-get-cd (package)
  "Open dired in the package directory."
  (interactive (list (el-get-read-package-name "cd to")))
  (el-get-error-unless-package-p package)
  (dired (el-get-package-directory package)))

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
      (with-temp-file (format "%s/%s.el" dir (el-get-source-name r))
	(insert (prin1-to-string r)))))
  (dired dir))

;;
;; notify user with emacs notifications API (new in 24)
;;
(when (and (eq system-type 'darwin)
	   (not (fboundp 'growl))
	   (file-executable-p el-get-growl-notify))
  (defun growl (title message)
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
	((fboundp 'growl)                (growl title message))
	(t                               (message "%s: %s" title message))))

(when (or (fboundp 'notifications-notify) (fboundp 'notify) (fboundp 'growl))
  (defun el-get-post-install-notification (package)
    "Notify the PACKAGE has been installed."
    (el-get-notify (format "%s installed" package)
		   "This package has been installed successfully by el-get."))
  (add-hook 'el-get-post-install-hooks 'el-get-post-install-notification)

  (defun el-get-post-update-notification (package)
    "Notify the PACKAGE has been updated."
    (el-get-notify (format "%s updated" package)
		   "This package has been updated successfully by el-get."))
  (add-hook 'el-get-post-update-hooks 'el-get-post-update-notification))

(defun el-get-post-init-message (package)
  "After PACKAGE init is done, just message about it"
  (message "el-get initialized package %s" package))

(add-hook 'el-get-post-init-hooks 'el-get-post-init-message)


;;
;; User Interface, Non Interactive part
;;
(defun el-get (&optional sync)
  "Check that all sources have been downloaded once, and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure the sources have been installed
and will set the load-path and Info-directory-list depending on
the el-get-sources setup.

el-get is also responsible for doing (require 'feature) for each
and every feature declared in `el-get-sources', so that it's
suitable for use in your emacs init script.

By default (SYNC is nil), `el-get' will run all the installs
concurrently so that you can still use Emacs to do your normal
work.

When SYNC is 'sync, each package will get installed one after the
other, and any error will stop it all.

When SYNC is 'wait, then `el-get' will enter a wait-loop and only
let you use Emacs once it has finished with its job. That's
useful an option to use in your `user-init-file'. Note that each
package in the list gets installed in parallel with this option.

Please note that the `el-get-init' part of `el-get' is always
done synchronously, so you will have to wait here. There's
`byte-compile' support though, and the packages you use are
welcome to use `autoload' too."
  (unless (or (null sync)
	      (member sync '(sync wait)))
    (error "el-get sync parameter should be either nil, sync or wait"))
  ;; If there's no autoload file, everything needs to be regenerated.
  (if (not (file-exists-p el-get-autoload-file)) (el-get-invalidate-autoloads))

  (let* ((p-status    (el-get-read-all-packages-status))
         (total       (length (el-get-package-name-list)))
         (installed   (el-get-count-package-with-status "installed"))
         (progress (and (eq sync 'wait)
                        (make-progress-reporter
			 "Waiting for `el-get' to complete… "
			 0 (- total installed) 0)))
         (el-get-default-process-sync sync))
    ;; keep the result of mapcar to return it even in the 'wait case
    (prog1
        (mapcar
         (lambda (source)
           (let* ((package (el-get-source-name source))
                  (status  (el-get-package-status package p-status)))
             ;; check if the package needs to be fetched (and built)
             (if (el-get-package-exists-p package)
                 (if (and status (string= "installed" status))
                     (condition-case err
                         (el-get-init package)
                       ((debug error) ;; catch-all, allow for debugging
                        (message "%S" (error-message-string err))))
                   (message "Package %s failed to install, remove it first." package))
               (el-get-install package))))
         el-get-sources)

      ;; el-get-install is async, that's now ongoing.
      (when progress
        (while (> (- total installed) 0)
          (sleep-for 0.2)
          ;; don't forget to account for installation failure
          (setq installed (el-get-count-package-with-status "installed" "required"))
          (progress-reporter-update progress (- total installed)))
        (progress-reporter-done progress))))

  ;; unless we have autoloads to update, just load them now
  (unless el-get-outdated-autoloads
    (el-get-eval-autoloads)))

(provide 'el-get)
