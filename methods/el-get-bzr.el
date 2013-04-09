;;; el-get --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.md file from the same distribution

(require 'el-get-core)

(defcustom el-get-bzr-branch-hook nil
  "Hook run after bzr branch."
  :group 'el-get
  :type 'hook)

(defun el-get-bzr-branch (package url post-install-fun)
  "Branch a given bzr PACKAGE following the URL using bzr."
  (let* ((bzr-executable (el-get-executable-find "bzr"))
	 (pname (el-get-as-string package))
	 (name  (format "*bzr branch %s*" package))
	 (ok    (format "Package %s installed" package))
	 (ko    (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,bzr-executable
		      :args ("branch" ,url ,pname)
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

(el-get-register-method :bzr
  :install #'el-get-bzr-branch
  :update #'el-get-bzr-pull
  :remove #'el-get-rmdir
  :install-hook el-get-bzr-branch-hook)

(provide 'el-get-bzr)
