;;; el-get.el
;;
;; Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.1
;; Created: 2010-06-17
;; Keywords: emacs elisp install elpa git apt-get debian
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  (require 'dim-installer)

(require 'dired-x) ; dired-make-relative-symlink

(defgroup el-get nil "el-get customization group"
  :group 'convenience)

(defcustom el-get-methods
  '(:git     (el-get-git-clone el-get-git-pull el-get-rmdir)
    :apt-get (el-get-apt-get-install el-get-apt-get-install el-get-apt-get-remove)
    :elpa    (el-get-elpa-install el-get-elpa-install el-get-rmdir))
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

Each package should be a cons '(name . (method . source)) where the
source syntax depends on the method.")

(defun el-get-check-init ()
  "Check that we can run el-get"
  (unless (file-directory-p el-get-dir)
    (make-directory el-get-dir)))

(defun el-get-package-exists-p (package)
  "true only when the given package name is either a directory or a symlink in el-get-dir"
  (let ((pdir (concat (file-name-as-directory el-get-dir) package)))
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

  (el-get-check-init)

  (message "el get %S" package)

  (let ((ret (shell-command-to-string 
	      (concat "cd " el-get-dir
		      " && " magit-git-executable " --no-pager clone " url))))

    (el-get-git-symlink-package package url)
    ret))

(defun el-get-git-pull (package url)
  "git pull the package"
  (let ((pdir (concat (file-name-as-directory el-get-dir) package)))
    (when (file-directory-p pdir)
      (shell-command-to-string 
       (concat "cd " pdir
	       " && " magit-git-executable " --no-pager pull " url)))))

(defun el-get-apt-get-install (package &optional url)
  "apt-get install package, url is there for API compliance"
  (shell-command-to-string 
   (concat "sudo " el-get-apt-get " install " package)))

(defun el-get-apt-get-remove (package &optional url)
  "apt-get remove package, url is there for API compliance"
  (shell-command-to-string 
   (concat "sudo " el-get-apt-get " remove " package)))

(defun el-get-elpa-install (package url)
  "ask elpa to install given package"
  (error "Not Implemented. Yet?"))

(defun el-get-rmdir (package url)
  "Just rm -fr the package directory"
  (error "Not Yet Implemented."))

(defun el-get ()
  "Check that all sources have been initialized and init them as needed"  
  (mapcar 
   (lambda (source)
     (let* ((package (symbol-name (car source)))
	    (method  (caadr source))
	    (url     (cdadr source))
	    (pdir   
	     (concat (file-name-as-directory el-get-dir) package)))
       (message "%s: %s %s %s" package method url (plist-get el-get-methods method))
       ;; if package already exists, pull it, method 1
       ;; if it does not already exists, init it, method 0
       (funcall 
	(nth (if (el-get-package-exists-p package) 1 0)
	     (plist-get el-get-methods method))
	package url)))
   el-get-sources))

(provide 'el-get)