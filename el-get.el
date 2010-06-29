;;; el-get.el
;;
;; Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.3
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git apt-get fink debian macosx
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  1. Install ELPA, see http://tromey.com/elpa/install.html
;;  2. (require 'el-get)
;;  3. then define your el-get-sources
;;  4. Use (el-get) from your .emacs or M-x el-get-install etc.
;;

(require 'package) ; that's ELPA

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defvar el-get-git-clone-hook       nil "Hook run after git clone.")
(defvar el-get-apt-get-install-hook nil "Hook run after apt-get install.")
(defvar el-get-fink-install-hook    nil "Hook run after fink install.")
(defvar el-get-elpa-install-hook    nil "Hook run after ELPA package install.")
(defvar el-get-http-install-hook    nil "Hook run after http retrieve.")

(defcustom el-get-methods
  '(:git     (:install el-get-git-clone 
		       :install-hook el-get-git-clone-hook
		       :update el-get-git-pull 
		       :remove el-get-rmdir)
    :apt-get (:install el-get-apt-get-install 
		       :install-hook el-get-apt-get-install-hook
		       :update el-get-apt-get-install
		       :remove el-get-apt-get-remove)
    :fink    (:install el-get-fink-install 
		       :install-hook el-get-fink-install-hook
		       :update el-get-fink-install
		       :remove el-get-fink-remove)
    :elpa    (:install el-get-elpa-install 
		       :install-hook el-get-elpa-install-hook
		       :update el-get-elpa-update
		       :remove el-get-rmdir)
    :http    (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
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

(defvar el-get-apt-get (executable-find "apt-get")
  "The apt-get executable.")

(defvar el-get-fink (executable-find "fink")
  "The fink executable.")

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
				(executable-find "install-info")))

(defvar el-get-sources nil
  "List of sources for packages.

Each source entry is a PLIST where the following properties are supported:

name

    The name of the package. It can be different from the name of
    the directory where the package is stored (after a `git
    clone' for example, in which case a symlink will be created.

type

    The type of the package, currently el-get offers support for
    `apt-get', `elpa', `git' and `http'. You can easily support
    your own types here, see the variable `el-get-methods'.

url

    Where to fetch the package, only meaningful for `git' and `http' types.

build

    Your build recipe gets there, often it looks like (\"./configure\" \"make\")

load-path

    This should be a list of directories you want `el-get' to add
    to your `load-path'.

info

    This string allows you to setup a directory where to find a
    'package.info' file, and it will even run `ginstall-info' for
    you to create the `dir' entry so that C-h i will be able to
    list the newly installed documentation. Note that you might
    need to C-x k your info buffer then C-h i again to be able to
    see the new menu entry.

load

    List of files to load, or a single file to load after having
    installed the source but before `require'ing its features.

features

    List of features el-get will `require' for you.
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
	 (fullpath (concat (file-name-as-directory pdir) path)))
    (unless (file-directory-p fullpath)
      (error "el-get could not find directory `%s' for package %s" path package))
    (add-to-list list fullpath)))

(defun el-get-package-exists-p (package)
  "true only when the given package name is either a directory or a symlink in el-get-dir"
  (let ((pdir (el-get-package-directory package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
	(file-symlink-p   pdir))))

;;
;; git support
;;
(defun el-get-git-dirname (url)
  "Get dirname from url"
  (car (split-string (substring url (string-match "[^/]+$" url)) "[.]")))

(defun el-get-git-symlink-package (package url)
  "The name we give to the package can be different from its main directory, ln -s"
  (let ((gitdir (el-get-git-dirname url)))
    (unless (el-get-package-exists-p package)
      (shell-command 
       (concat "cd " el-get-dir 
	       " && ln -s \"" gitdir "\" \"" package "\"")))))

(defun el-get-git-clone (package url)
  "clone the given package following the url"
  (let ((git-executable (if (file-executable-p magit-git-executable)
			    magit-git-executable
			  (executable-find "git"))))
    (unless (file-executable-p git-executable)
      (error "el-get-git-clone requires `magit-git-executable` to be set, or the binary `git' to be found in your PATH"))

    (let ((ret 
	   (shell-command-to-string 
	    (concat "cd " el-get-dir " && " git-executable " --no-pager clone " url))))

      (el-get-git-symlink-package package url)
      (run-hooks 'el-get-git-clone-hook)
      ret)))

(defun el-get-git-pull (package url)
  "git pull the package"
  (let ((pdir (el-get-package-directory package)))
    (when (file-directory-p pdir)
      (shell-command-to-string 
       (concat "cd " pdir
	       " && " magit-git-executable " --no-pager pull " url)))))

;;
;; apt-get support
;;
(defun el-get-sudo-shell-command-to-string (command) 
  "Ask a password then use sudo, when required"
  (let* ((sudo-test (shell-command-to-string "sudo echo el-get"))
	 (sudo-pass (if (string= sudo-test "el-get\n")
			nil
		      (read-passwd "sudo password: "))))
    (shell-command-to-string
     (concat (when sudo-pass (concat "echo " sudo-pass " | "))
	     "sudo -S " command))))

(defun el-get-apt-get-symlink (package)
  "ln -s /usr/share/emacs/site-lisp/package ~/.emacs.d/el-get/package"
  (let ((debdir (concat "/usr/share/emacs/site-lisp/" package)))
    (shell-command 
     (concat "cd " el-get-dir " && ln -s " debdir  " " package))))

(defun el-get-apt-get-package-status (package)
  "returns the package status from dpkg --get-selections"
  (substring 
   (shell-command-to-string 
    (format
     "dpkg -l %s| awk '/^ii/ && $2 = \"%s\" {print \"ok\"}'" package package)) 0 -1))

(defun el-get-apt-get-install (package &optional url)
  "apt-get install package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " install " package)))
  (unless (string= (el-get-apt-get-package-status package) "install")
    (error "Error: apt-get reports package %s status is not \"install\"" package))
  (el-get-apt-get-symlink package)
  (run-hooks 'el-get-apt-get-install-hook)
  nil)

(defun el-get-apt-get-remove (package &optional url)
  "apt-get remove package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " remove -y " package))))

;;
;; fink support
;;
(defun el-get-fink-install (package &optional url)
  "fink install package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat "echo Y | " el-get-fink " install " package)))
  (run-hooks 'el-get-fink-install-hook)
  nil)

(defun el-get-fink-remove (package &optional url)
  "fink remove package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-fink " remove -r " package))))

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

(defun el-get-elpa-install (package &optional url)
  "ask elpa to install given package"
  (let ((elpa-dir (el-get-elpa-package-directory package)))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      (package-install (intern-soft package)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package)))
  
(defun el-get-elpa-update (package &optional url)
  "ask elpa to update given package"
  (el-get-rmdir (concat (file-name-as-directory package-user-dir) package))
  (package-install (intern-soft package)))

;;
;; http support
;;
(defun el-get-http-retrieve (package url)
  "Get an elisp file using `url-retrieve-synchronously'. 

Returns the feature provided if any"
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (concat (file-name-as-directory pdir) package ".el")))
    (with-current-buffer (url-retrieve-synchronously url)
      ;; prune HTTP headers before save
      (beginning-of-buffer)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file dest))))

(defun el-get-http-install (package url)
  "Dowload a single-file package over HTTP "
  (let ((pdir   (el-get-package-directory package)))
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (el-get-http-retrieve package url)))

;;
;; Common support bits
;;
(defun el-get-rmdir (package url)
  "Just rm -rf the package directory. Follow symlinks."
  (error "Not Yet Implemented."))

(defun el-get-build (package commands &optional subdir)
  "run each command from the package directory"
  (let* ((pdir   (el-get-package-directory package))
	 (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir)))
    (dolist (c commands)
      (message "el-get %s: cd %s && %s" package wdir c)
      (message "%S" (shell-command-to-string 
		     (concat "cd " wdir " && " c))))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for given package"
  (dolist (s el-get-sources source)
    (when (string= package (format "%s" (plist-get s :name)))
      (setq source s))))

;;
;; User Interface, Interactive part
;;
(defun el-get-read-package-name (action &optional package)
  "Ask user for a package name in minibuffer, with completion.

When given a package name, check for its existence"
  (let ((plist 
	  (mapcar (lambda (x) (format "%s" (plist-get x :name))) el-get-sources)))
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
	 (pdir     (el-get-package-directory package)))

    ;; apt-get and ELPA will take care of load-path, Info-directory-list
    (unless (member method '(elpa apt-get))
      ;; append entries to load-path and Info-directory-list
      (mapc (lambda (path) 
	      (el-get-add-path-to-list package 'load-path path))
	    (if (stringp el-path) (list el-path) el-path))

      (when (and infodir
		 (file-directory-p (concat (file-name-as-directory pdir) infodir)))
	(require 'info)
	(info-initialize)
	;; add to Info-directory-list
	(el-get-add-path-to-list package 'Info-directory-list infodir)
	;; build the infodir entry, too
	(el-get-build
	 package
	 `(,(format "%s %s.info dir" el-get-install-info package)) infodir)))

    ;; loads
    (when loads
      (mapc (lambda (file) 
	      (let ((pfile (concat (file-name-as-directory pdir) file)))
		(unless (file-exists-p pfile)
		  (error "el-get could not find file '%s'" pfile))
		(message "load '%s'" pfile)
		(load pfile)))
	    (if (stringp loads) (list loads) loads)))

    ;; features, only ELPA will handle them on its own
    (unless (eq method 'elpa)
      ;; if a feature is provided, require it now
      (when feats 
	(mapc (lambda (feature) (message "require '%s" (require feature)))
	      (if (symbolp feats) (list feats) feats))))

      ;; return the package
      package))

(defun el-get-install (&optional package)
  "Install given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (install  (el-get-method method :install))
	 (hooks    (el-get-method method :install-hook))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))

    ;; check we can install the package
    (el-get-check-init)

    ;; and install the package now
    (message "el-get install %s" package)
    (funcall install package url)
    (run-hooks hooks)

    ;; consider building when sources are thus setup
    (el-get-build package commands)

    ;; and init
    (el-get-init package)))

(defun el-get-update (&optional package)
  "Update given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (update   (el-get-method method :update))
	 (url      (plist-get source :url))
	 (commands (plist-get source :build)))
    ;; update the package now
    (message "el-get update %s" package)
    (funcall update package url)

    ;; consider building when sources are thus setup
    (el-get-build package commands)))

(defun el-get-remove (&optional package)
  "Remove given package. Read the package name with completion when not given."
  (interactive)
  (let* ((package (or package (el-get-read-package-name "Install" package)))
	 (source   (el-get-package-def package))
	 (method   (plist-get source :type))
	 (remove   (el-get-method method :remove))
	 (url      (plist-get source :url)))
    ;; remove the package now
    (message "el-get remove %s" package)
    (funcall remove package url)))

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
     (let* ((package  (format "%s" (plist-get source :name))))
       ;; check if the package needs to be fetched (and built)
       (if (el-get-package-exists-p package)
	   (el-get-init package)
	 (el-get-install package))))
   el-get-sources))

(provide 'el-get)
