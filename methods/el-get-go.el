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

(defcustom el-get-go (executable-find "go")
  "The go executable."
  :group 'el-get
  :type 'file)

(defcustom el-get-go-install-hook nil
  "Hook run after go install."
  :group 'el-get
  :type 'hook)

(defun el-get-go-install (package url post-install-fun)
  "go install PACKAGE"
  (let* ((gopath (getenv "GOPATH"))
         (source (el-get-package-def package))
         (pkgname (el-get-as-string (plist-get source :pkgname)))
         (pdir (el-get-package-directory package))
         (name (format "*go get %s*" package))
         (ok   (format "Package %s installed." package))
         (ko   (format "Could not install package %s." package)))
    ;; TODO: no idea how to check this for insecure connections
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (setenv "GOPATH" pdir)
    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,(el-get-executable-find "go")
                      :args ("get" "-v" "-u" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-install-fun)
    (setenv "GOPATH" gopath)))

(el-get-register-method :go
  :install #'el-get-go-install
  :update #'el-get-go-install
  :remove #'el-get-rmdir
  :install-hook 'el-get-go-install-hook)

(provide 'el-get-go)
