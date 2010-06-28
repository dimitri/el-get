;;; el-get.el
;;
;; Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.2
;; Created: 2010-06-17
;; Keywords: emacs elisp install elpa git apt-get debian
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  (require 'el-get)
;;  then define your el-get-sources

(require 'dired-x) ; dired-make-relative-symlink

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defvar el-get-git-clone-hook       nil "Hook run after git clone.")
(defvar el-get-apt-get-install-hook nil "Hook run after apt-get install.")
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
    :elpa    (:install el-get-elpa-install 
		       :install-hook el-get-elpa-install-hook
		       :update el-get-elpa-install
		       :remove el-get-rmdir)
    :http    (:install el-get-http-install
		       :install-hook el-get-http-install-hook
		       :update el-get-http-install
		       :remove el-get-rmdir))
  "Register methods that el-get can use to fetch and update a given package

That should be a plist of method names whose properties is a list
of 3 functions. The first one is the method to init the package,
the second to update the package and the third to remove it."
  :type '(repeat (cons symbol function))
  :group 'el-get)

(defvar el-get-dir "~/.emacs.d/el-get/"
  "Define where to fetch the packages.")

(defvar el-get-apt-get (or (executable-find "apt-get")
			   (executable-find "fink"))
  "The apt-get executable.")

(defvar el-get-sources nil
  "List of sources for packages.

An example of each package entry is following.

	(:name 'bbdb
	       :type git
	       :url \"git://github.com/barak/BBDB.git\"
	       :load-path '(\"./lisp\" \"./bits\")
	       :info \"texinfo\"
	       :build '(\"./configure\" \"make\"))")

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
  (let ((pdir (el-get-package-dir package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
	(file-symlink-p   pdir))))

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
  (unless (file-executable-p magit-git-executable)
    (error "el-get-git-clone requires `magit-git-executable` to be set"))

  (let ((ret (shell-command-to-string 
	      (concat "cd " el-get-dir
		      " && " magit-git-executable " --no-pager clone " url))))

    (el-get-git-symlink-package package url)
    (run-hooks 'el-get-git-clone-hook)
    ret))

(defun el-get-git-pull (package url)
  "git pull the package"
  (let ((pdir (el-get-package-directory package)))
    (when (file-directory-p pdir)
      (shell-command-to-string 
       (concat "cd " pdir
	       " && " magit-git-executable " --no-pager pull " url)))))

(defun el-get-sudo-shell-command-to-string (command) 
  "Ask a password then use sudo, when required"
  (let* ((sudo-test (shell-command-to-string "sudo echo el-get"))
	 (sudo-pass (if (string= sudo-test "el-get\n")
			nil
		      (read-passwd "sudo password: "))))
    (shell-command-to-string 
     (concat (when sudo-pass (concat "echo " sudo-pass " | "))
	     "sudo -S " command))))

(defun el-get-apt-get-install (package &optional url)
  "apt-get install package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " install " package)))
  (run-hooks 'el-get-apt-get-install-hook)
  nil)

(defun el-get-apt-get-remove (package &optional url)
  "apt-get remove package, url is there for API compliance"
  ;; this can be somewhat chatty but I guess you want to know about it
  (message "%S" (el-get-sudo-shell-command-to-string 
		 (concat el-get-apt-get " remove -y " package))))

(defun el-get-elpa-install (package url)
  "ask elpa to install given package"
  (error "Not Implemented. Yet?"))

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

(defun el-get-rmdir (package url)
  "Just rm -fr the package directory"
  (error "Not Yet Implemented."))

(defun el-get-build (package commands)
  "run each command from the package directory"
  (let ((pdir   (el-get-package-directory package)))
    (dolist (c commands)
      (message "el-get %s: cd %s && %s" package pdir c)
      (message "%S" (shell-command-to-string 
		     (concat "cd " pdir " && " c))))))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for given package"
  (dolist (s el-get-sources source)
    (when (string= package (format "%s" (plist-get s :name)))
      (setq source s))))

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
	 (feats    (plist-get source :features))
	 (el-path  (or (plist-get source :load-path) '(".")))
	 (infodir  (plist-get source :info)))

    ;; ELPA will take care of load-path, Info-directory-list and features
    (unless (eq method 'elpa)
      ;; append entries to load-path and Info-directory-list
      (mapc (lambda (path) 
	      (el-get-add-path-to-list package 'load-path path))
	    (if (stringp el-path) (list el-path) el-path))

      (mapc (lambda (path) 
	      (el-get-add-path-to-list package 'Info-directory-list path))
	    (if (stringp infodir) (list infodir) infodir))

      ;; if a feature is provided, require it now
      (when feats 
	(mapc (lambda (feature) (message "require '%s" (require feature)))
	      (if (symbolp feats) (list feats) feats)))

      ;; return the package
      package)))

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
	   (el-get-install package)
	 (el-get-init package))))
   el-get-sources))

(provide 'el-get)
