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
(require 'el-get-custom)

(defcustom el-get-darcs-get-hook nil
  "Hook run after darcs get."
  :group 'el-get
  :type 'hook)

(defun el-get-darcs-get (package url post-install-fun)
  "Get a given PACKAGE following the URL using darcs."
  (let* ((darcs-executable (el-get-executable-find "darcs"))
         (pname (el-get-as-string package))
         (name  (format "*darcs get %s*" package))
         (ok    (format "Package %s installed" package))
         (ko    (format "Could not install package %s." package)))
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,darcs-executable
                      :args ("get" "--lazy" ,url ,pname)
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
    (el-get-insecure-check package url)

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

(el-get-register-method :darcs
  :install #'el-get-darcs-get
  :update #'el-get-darcs-pull
  :remove #'el-get-rmdir
  :install-hook 'el-get-darcs-get-hook)

(provide 'el-get-darcs)
