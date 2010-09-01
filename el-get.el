;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.9
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs apt-get fink http http-tar
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.asciidoc file from the same distribution
;;
;; Changelog
;;
;;  0.10 - <WIP> - Can I haz your recipes?
;;
;;   - Implement el-get recipes so that el-get-sources can be a simple list
;;     of symbols. Now that there's an authoritative git repository, where
;;     to share the recipes is easy.
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

(eval-when-compile 
  ;; yes we do need the loop facility, for merging 2 property lists
  (require 'cl))

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defvar el-get-git-clone-hook        nil "Hook run after git clone.")
(defvar el-get-git-svn-clone-hook    nil "Hook run after git svn clone.")
(defvar el-get-bzr-branch-hook       nil "Hook run after bzr branch.")
(defvar el-get-cvs-checkout-hook     nil "Hook run after cvs checkout.")
(defvar el-get-apt-get-install-hook  nil "Hook run after apt-get install.")
(defvar el-get-apt-get-remove-hook   nil "Hook run after apt-get remove.")
(defvar el-get-fink-install-hook     nil "Hook run after fink install.")
(defvar el-get-fink-remove-hook      nil "Hook run after fink remove.")
(defvar el-get-elpa-install-hook     nil "Hook run after ELPA package install.")
(defvar el-get-http-install-hook     nil "Hook run after http retrieve.")
(defvar el-get-http-tar-install-hook nil "Hook run after http-tar package install.")

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
    :cvs     (:install el-get-cvs-checkout
		       :install-hook el-get-cvs-checkout-hook
		       :update el-get-cvs-update
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
		       :remove el-get-rmdir)
    :http    (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
		       :remove el-get-rmdir)
    :http-tar (:install el-get-http-tar-install
		       :install-hook el-get-http-tar-install-hook
		       :update el-get-http-tar-install
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

(defvar el-get-apt-get (executable-find "apt-get")
  "The apt-get executable.")

(defvar el-get-apt-get-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended.")

(defvar el-get-fink (executable-find "fink")
  "The fink executable.")

(defvar el-get-fink-base "/sw/share/doc"
  "Where to link the el-get symlink to, /<package> will get appended.")

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
				(executable-find "install-info")))

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

:type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git' and `http'. You can easily support
    your own types here, see the variable `el-get-methods'.

:url

    Where to fetch the package, only meaningful for `git' and `http' types.

:build

    Your build recipe gets there, often it looks like (\"./configure\" \"make\")

:load-path

    This should be a list of directories you want `el-get' to add
    to your `load-path'.

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

    Currently only used by the http-tar support for you to give
    the tar options you want to use. Typically would be \"xzf\",
    but you might want to choose \"xjf\" for handling .tar.bz
    files e.g.

:module

    Currently only used by the `csv' support, allow you to
    configure the module you want to checkout in the given URL.

:after

    A function to run once `el-get' is done with `el-get-init',
    can be a lambda.
")


(defun el-get-method (method-name action)
  "return the function to call for doing action (e.g. install) in given method"
  (let* ((method  (intern-soft (concat ":" (format "%s" method-name))))
	 (actions (plist-get el-get-methods method)))
    (plist-get actions action)))

(defun el-get-check-init ()
  "Check that we can run el-get"
  (unless (file-directory-p el-get-dir)
    (make-directory el-get-dir)))

(defun el-get-package-directory (package)
  "Returns the package installation directory absolute name"
  (concat (file-name-as-directory el-get-dir) package))

(defun el-get-add-path-to-list (package list path)
  "(add-to-list list path) checking for path existence within
given package directory."
  (let* ((pdir     (el-get-package-directory package))
	 (fullpath (expand-file-name (concat (file-name-as-directory pdir) path))))
    (unless (file-directory-p fullpath)
      (error "el-get could not find directory `%s' for package %s, at %s" path package fullpath))
    (add-to-list list fullpath)))

(defun el-get-package-exists-p (package)
  "true only when the given package name is either a directory or a symlink in el-get-dir"
  (let ((pdir (el-get-package-directory package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
	(file-symlink-p   pdir))))


;;
;; call-process-list utility, to do same as bash && feature
;;
(defun el-get-start-process-list-sentinel (proc change)
  "When proc has exited and was successful, chain next command"
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cname   (process-get proc :command-name))
	  (cbuf    (process-get proc :buffer-name))
	  (message (process-get proc :message))
	  (errorm  (process-get proc :error))
	  (package (process-get proc :el-get-package))
	  (final-f (process-get proc :el-get-final-func))
	  (next    (process-get proc :el-get-start-process-list)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) cbuf))
	    (error "el-get: %s" cname errorm))
	(message "el-get: %s" message))
      
      (when cbuf (kill-buffer cbuf))
      (if next
	  (el-get-start-process-list package next final-f)
	(when (functionp final-f)
	  (funcall final-f package))))))

(defun el-get-start-process-list (package commands final-func)
  "run each command one after the other, in order, stopping at
first error.

commands should be a list of plists with at least the following
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
	   (startf  (if shell #'start-process-shell-command #'start-process))
	   (default-directory (if cdir 
				  (file-name-as-directory 
				   (expand-file-name cdir))
				default-directory))
	   (proc    (apply startf cname cbuf program args)))

      ;; add the properties to the process, then set the sentinel
      (mapc (lambda (x) (process-put proc x (plist-get c x))) c)
      (process-put proc :el-get-package package)
      (process-put proc :el-get-final-func final-func)
      (process-put proc :el-get-start-process-list (cdr commands))
      (set-process-sentinel proc 'el-get-start-process-list-sentinel)
      (when filter (set-process-filter proc filter)))))


;;
;; git support
;;
(defun el-get-git-executable ()
  "return git executable to use, or signal an error when not found"
  (let ((git-executable (if (and (boundp 'magit-git-executable)
				 (file-executable-p magit-git-executable))
			    magit-git-executable
			  (executable-find "git"))))
    (unless (file-executable-p git-executable)
      (error 
       (concat "el-get-git-clone requires `magit-git-executable' to be set, "
	       "or the binary `git' to be found in your PATH")))
    git-executable))

(defun el-get-git-clone (package url post-install-fun)
  "clone the given package following the url"
  (let* ((git-executable (el-get-git-executable))
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
		      :error ,ko))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package"
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
		      :error ,ko))
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
  "bzr pull the package"
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


(defun el-get-cvs-checkout (package url post-install-fun)
  "cvs checkout the package"
  (let* ((cvs-executable (executable-find "cvs"))
	 (source  (el-get-package-def package))
	 (module  (plist-get source :module))
	 (name    (format "*cvs checkout %s*" package))
	 (ok      (format "Checked out package %s." package))
	 (ko      (format "Could not checkout package %s." package)))

    (message "%S" `(:args ("-d" ,url "checkout" "-d" ,package ,module)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,cvs-executable
		      :args ("-d" ,url "checkout" "-d" ,package ,module)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-cvs-update (package url post-update-fun)
  "cvs checkout the package"
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
;; utilities for both apt-get and fink support (dpkg based)
;;
(defun el-get-dpkg-package-status (package)
  "returns the package status from dpkg --get-selections"
  (substring 
   (shell-command-to-string 
    (format
     "dpkg -l %s| awk '/^ii/ && $2 = \"%s\" {print \"ok\"}'" package package)) 0 -1))

;;
;; those functions are meant as hooks at install and remove, and they will
;; get the global value of package, which has been set before calling
;; run-hooks.
;;
(defun el-get-dpkg-symlink ()
  "ln -s /usr/share/emacs/site-lisp/package ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package))
	 (method  (plist-get source :type))
	 (basedir (cond ((eq method 'apt-get) el-get-apt-get-base)
			((eq method 'fink)    el-get-fink-base)))
	 (debdir  (concat (file-name-as-directory basedir) package)))
    (unless (file-directory-p pdir)
      (shell-command 
       (concat "cd " el-get-dir " && ln -s " debdir  " " package)))))

(defun el-get-dpkg-remove-symlink ()
  "rm -f ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package)))
    (message "PHOQUE %S" pdir)
    (when (file-symlink-p pdir)
      (message (concat "cd " el-get-dir " && rm -f " package))
      (shell-command 
       (concat "cd " el-get-dir " && rm -f " package)))))


;;
;; apt-get support
;;
(add-hook 'el-get-apt-get-install-hook 'el-get-dpkg-symlink)

(defun el-get-sudo-password-process-filter (proc string)
  "Filter function that fills the process buffer's and matches a password prompt"
  (unless (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      ;; arrange to remember already seen content
      (unless (boundp 'el-get-sudo-password-process-filter-pos)
	(make-local-variable 'el-get-sudo-password-process-filter-pos)
	(setq el-get-sudo-password-process-filter-pos (point-min)))

      (save-excursion
	(goto-char (point-max))
	(insert string)
	;; redirect the subprocess sudo prompt to the user face, and answer it
	(goto-char el-get-sudo-password-process-filter-pos)
	(while (re-search-forward "password" nil t)
	  (let* ((prompt (thing-at-point 'line))
		 (pass   (read-passwd prompt)))
	    (process-send-string proc (concat pass "\n"))))
	(setq el-get-sudo-password-process-filter-pos (point-max))))))

(defun el-get-apt-get-install (package url post-install-fun)
  "echo $pass | sudo -S apt-get install package"
  (let* ((name (format "*apt-get install %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))
	 
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "apt-get") "install" ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-apt-get-remove (package url post-remove-fun)
  "apt-get remove package, url is there for API compliance"
  (let* ((name (format "*apt-get remove %s*" package))
	 (ok   (format "Package %s removed." package))
	 (ko   (format "Could not remove package %s." package)))
	 
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "apt-get") "remove" "-y" ,package)
		      :message ,ok
		      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-apt-get-remove-hook 'el-get-dpkg-remove-symlink)


;;
;; fink support
;;
(defun el-get-fink-install (package url post-install-fun)
  "sudo -S fink install package"
  (let* ((name (format "*fink install %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))
	 
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "fink") "install" ,package)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(add-hook 'el-get-fink-install-hook 'el-get-dpkg-symlink)

(defun el-get-fink-remove (package url post-remove-fun)
  "apt-get remove package, url is there for API compliance"
  (let* ((name (format "*fink remove %s*" package))
	 (ok   (format "Package %s removed." package))
	 (ko   (format "Could not remove package %s." package)))
	 
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,(executable-find "fink") "-y" "remove" ,package)
		      :message ,ok
		      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-fink-remove-hook 'el-get-dpkg-remove-symlink)


;;
;; ELPA support
;;
(defun el-get-elpa-package-directory (package)
  "return the directory where ELPA stores a package, or nil if
package isn't currently installed by ELPA."
  (let* ((pname (format "%s" package))  ; easy way to cope with symbols etc.

	 (l
	  ;; we use try-completion to find the realname of the directory
	  ;; ELPA used, and this wants an alist, we trick ls -i -1 into
	  ;; that.
	  (mapcar 'split-string
		  (split-string
		   (shell-command-to-string
		    (concat "ls -i1 "
			    (file-name-as-directory package-user-dir))))))

	 (realname (try-completion pname l)))

    (if realname (concat (file-name-as-directory package-user-dir) realname) 
      realname)))

(defun el-get-elpa-symlink-package (package)
  "ln -s ~/.emacs.d/elpa/<package> ~/.emacs.d/el-get/<package>"
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (el-get-package-exists-p package)
      (shell-command
       (concat "cd " el-get-dir 
	       " && ln -s \"" elpa-dir "\" \"" package "\"")))))

(defun el-get-elpa-install (package url post-install-fun)
  "ask elpa to install given package"
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      (package-install (intern-soft package)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package))
  (funcall post-install-fun package))

(defun el-get-elpa-update (package url post-update-fun)
  "ask elpa to update given package"
  (el-get-rmdir (concat (file-name-as-directory package-user-dir) package nil))
  (package-install (intern-soft package))
  (funcall post-install-fun package))


;;
;; http support
;;
(defun el-get-http-retrieve-callback (url-arg package post-install-fun &optional dest)
  "callback function for `url-retrieve', store the emacs lisp file for the package.

url-arg is nil in my tests but url-retrieve seems to insist on
passing it the the callback function nonetheless."
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (or dest (concat (file-name-as-directory pdir) package ".el")))
	 (part   (concat dest ".part")))
    ;; prune HTTP headers before save
    (beginning-of-buffer)
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (write-file part)
    (rename-file part dest)
    (message "Renamed to %s" dest))
  (funcall post-install-fun package))

(defun el-get-http-install (package url post-install-fun &optional dest)
  "Dowload a single-file package over HTTP and store it in dest, or in package.el"
  (let ((pdir   (el-get-package-directory package)))
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (url-retrieve 
     url 'el-get-http-retrieve-callback `(,package ,post-install-fun ,dest))))


;;
;; http-tar support (archive)
;;
(defun el-get-http-tar-cleanup-extract-hook ()
  "Cleanup after tar xzf: if there's only one subdir, move all the files up"
  (let* ((pdir    (el-get-package-directory package))
	 (url     (plist-get source :url))
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
  "Dowload a tar archive package over HTTP "
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
		  (el-get-start-process-list
		   package
		   '((:command-name ,name
				    :buffer-name ,name
				    :default-directory ,pdir
				    :program ,(executable-find "tar")
				    :args (,@options ,tarfile)
				    :message ,ok
				    :error ,ko))
		   ,(symbol-function post-install-fun)))))
    (el-get-http-install package url post dest)))

(add-hook 'el-get-http-tar-install-hook 'el-get-http-tar-cleanup-extract-hook)


;;
;; Common support bits
;;
(defun el-get-rmdir (package url post-remove-fun)
  "Just rm -rf the package directory. Follow symlinks."
  (dired-delete-file (el-get-package-directory package) 'always)
  (funcall post-remove-fun package))

(defun el-get-build (package commands &optional subdir sync post-build-fun)
  "run each command from the package directory"
  (let* ((pdir   (el-get-package-directory package))
	 (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir))
	 (buf    (format "*el-get-build: %s*" package)) 
	 (default-directory wdir))
    (if sync
	(progn
	  (dolist (c commands)
	    (message "el-get %s: cd %s && %s" package wdir c)
	    (message "%S" (shell-command-to-string 
			   (concat "cd " wdir " && " c))))
	  (when (and post-build-fun (functionp post-build-fun))
	    (funcall post-build-fun)))

      ;; async
      (let ((process-list
	     (mapcar (lambda (c)
		       (let* ((split    (split-string c))
			      (name     (car split))
			      (program  (if (file-executable-p (expand-file-name name))
					    (expand-file-name name)
					  (executable-find name)))
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

(defun el-get-read-recipe (package)
  "Return the package source definition for package, from the recipes"
  (loop for dir in el-get-recipe-path
	for recipe = (concat (file-name-as-directory dir) package ".el")
	if (file-exists-p recipe)
	return (car (with-temp-buffer 
		      (insert-file-contents-literally recipe)
		      (read-from-string (buffer-string))))))

(defun el-get-source-name (source)
  "Return the package name (stringp) given an `el-get-sources' entry"
  (if (symbolp source) (symbol-name source)
    (format "%s" (plist-get source :name))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for given package"
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
;; User Interface, Interactive part
;;
(defun el-get-read-package-name (action &optional package)
  "Ask user for a package name in minibuffer, with completion.

When given a package name, check for its existence"
  (let ((plist 
	  (mapcar (lambda (x) 
		    (if (symbolp x) (format "%S" x)
		      (format "%s" (plist-get x :name))))
		  el-get-sources)))
    (if package
	(unless (member package plist)
	  (error "el-get: can not find package name `%s' in `el-get-sources'" package))
      (completing-read (format "%s package: " action) plist))))

(defun el-get-init (&optional package)
  "Care about load-path, Info-directory-list, and (require 'features)"
  (interactive)
  (let* ((package  (or package (el-get-read-package-name "Init" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (loads    (plist-get source :load))
	 (feats    (plist-get source :features))
	 (el-path  (or (plist-get source :load-path) '(".")))
	 (infodir  (plist-get source :info))
	 (after    (plist-get source :after))
	 (pdir     (el-get-package-directory package)))

    ;; apt-get and ELPA will take care of load-path, Info-directory-list
    (unless (member method '(elpa apt-get fink))
      ;; append entries to load-path and Info-directory-list
      (mapc (lambda (path) 
	      (el-get-add-path-to-list package 'load-path path))
	    (if (stringp el-path) (list el-path) el-path))

      (let* ((infodir-abs-conf (concat (file-name-as-directory pdir) infodir))
	     (infodir-abs (if (file-directory-p infodir-abs-conf)
			      infodir-abs-conf
			    (file-name-directory infodir-abs-conf)))
	     (infodir-rel (if (file-directory-p infodir-abs-conf)
			      infodir
			    (file-name-directory infodir)))
	     (infofile (if (and (file-exists-p infodir-abs-conf)
				(not (file-directory-p infodir-abs-conf)))
			   infodir-abs-conf
			 (concat (file-name-as-directory infodir-abs) package))))

	(when (and infodir (file-directory-p infodir-abs))
	  (require 'info)
	  (info-initialize)
	  ;; add to Info-directory-list
	  (el-get-add-path-to-list package 'Info-directory-list infodir-rel)
	  ;; build the infodir entry, too
	  (el-get-build
	   package
	   `(,(format "%s %s.info dir" el-get-install-info infofile)) infodir-rel t))))

    ;; loads
    (when loads
      (mapc (lambda (file) 
	      (let ((pfile (concat (file-name-as-directory pdir) file)))
		(unless (file-exists-p pfile)
		  (error "el-get could not find file '%s'" pfile))
		(message "el-get: load '%s'" pfile)
		(load pfile)))
	    (if (stringp loads) (list loads) loads)))

    ;; features, only ELPA will handle them on its own
    (unless (eq method 'elpa)
      ;; if a feature is provided, require it now
      (when feats 
	(mapc (lambda (feat)
		(let ((feature (if (stringp feat) (intern-soft feat) feat)))
		  (message "require '%s" (require feature))))
	      (cond ((symbolp feats) (list feats))
		    ((stringp feats) (list (intern-soft feats)))
		    (t feats)))))

    ;; call the "after" user function
    (when (and after (functionp after))
      (message "el-get: Calling init user function for package %s" package)
      (funcall after))

    ;; return the package
    package))

(defun el-get-post-install (package)
  "Post install a package. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (hooks    (el-get-method (plist-get source :type) :install-hook))
	 (commands (plist-get source :build)))
    ;; post-install is the right place to run install-hook
    (run-hooks hooks)
    ;; build then init
    (el-get-build package commands nil nil (lambda (package) (el-get-init package)))))

(defun el-get-install (&optional package)
  "Install given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (install  (el-get-method method :install))
	 (url      (plist-get source :url)))

    ;; check we can install the package
    (el-get-check-init)

    ;; and install the package now
    (message "el-get install %s" package)
    (funcall install package url 'el-get-post-install)))

(defun el-get-post-update (package)
  "Post update a package. This will get run by a sentinel."
  (let* ((source   (el-get-package-def package))
	 (commands (plist-get source :build)))
    (el-get-build package commands nil nil 
		  (lambda (package) (message "el-get-post-update %s: done" package)))))

(defun el-get-update (&optional package)
  "Update given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Update" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (update   (el-get-method method :update))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))
    ;; update the package now
    (message "el-get update %s" package)
    (funcall update package url 'el-get-post-update)))

(defun el-get-post-remove (package)
  "run the post-remove hooks"
  (let* ((source  (el-get-package-def package))
	 (hooks   (el-get-method (plist-get source :type) :remove-hook)))
    (when hooks
      (run-hooks hooks))))

(defun el-get-remove (&optional package)
  "Remove given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Remove" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (remove   (el-get-method method :remove))
	 (url      (plist-get source :url)))
    ;; remove the package now
    (message "el-get remove %s" package)
    (funcall remove package url 'el-get-post-remove)))

(defun el-get-cd (&optional package)
  "Open dired in the package directory."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "cd to" package)))
	 (pdir    (el-get-package-directory package)))
    (dired pdir)))


;;
;; User Interface, Non Interactive part
;;
(defun el-get ()
  "Check that all sources have been downloaded once, and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure the sources have been installed
and will set the load-path and Info-directory-list depending on
the el-get-sources setup.

el-get is also responsible for doing (require 'feature) for each
and every feature declared in `el-get-sources', so that it's
suitable for use in your emacs init script.
"
  (mapcar 
   (lambda (source)
     (let* ((package (el-get-source-name source)))
       ;; check if the package needs to be fetched (and built)
       (if (el-get-package-exists-p package)
	   (el-get-init package)
	 (el-get-install package))))
   el-get-sources))

(provide 'el-get)
