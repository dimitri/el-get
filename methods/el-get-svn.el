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
(require 'el-get-recipes)

(defcustom el-get-svn (executable-find "svn")
  "The svn executable."
  :group 'el-get
  :type 'file)

(defcustom el-get-svn-checkout-hook nil
  "Hook run after svn checkout."
  :group 'el-get
  :type 'hook)

;;
;; svn support
;;
(defun el-get-svn-checkout (package url post-install-fun)
  "svn checkout the package."
  (let* ((svn-executable (el-get-executable-find "svn"))
         (source  (el-get-package-def package))
         (pname   (el-get-as-string package))
         (name    (format "*svn checkout %s*" package))
         (ok      (format "Checked out package %s." package))
         (ko      (format "Could not checkout package %s." package)))
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,svn-executable
                      :args ("checkout" ,url ,pname)
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
    (el-get-insecure-check package url)

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

(el-get-register-method :svn
  :install #'el-get-svn-checkout
  :update #'el-get-svn-update
  :remove #'el-get-rmdir
  :install-hook 'el-get-svn-checkout-hook)

(provide 'el-get-svn)
