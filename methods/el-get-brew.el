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

(require 'el-get-apt-get)

(defcustom el-get-brew (executable-find "brew")
  "The brew executable."
  :group 'el-get
  :type 'directory)

(defcustom el-get-brew-base "/usr/local/bin"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'file)

(defcustom el-get-brew-install-hook nil
  "Hook run after brew install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-brew-remove-hook nil
  "Hook run after brew remove."
  :group 'el-get
  :type 'hook)

(defun el-get-brew-install (package url post-install-fun)
  "brew install PACKAGE"
  (let* ((name (format "*brew install %s*" package))
         (pkgname (el-get-as-string package))
         (ok   (format "Package %s installed." package))
         (ko   (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,(el-get-executable-find "brew")
                      :args ("install" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-install-fun)))

(add-hook 'el-get-brew-install-hook 'el-get-dpkg-symlink)

(defun el-get-brew-update (package url post-update-fun)
  "brew update PACKAGE"
  (let* ((name (format "*brew update %s*" package))
         (pkgname (el-get-as-string package))
         (ok   (format "Package %s updated." package))
         (ko   (format "Could not update package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,(el-get-executable-find "brew")
                      :args ("upgrade" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-update-fun)))

(defun el-get-brew-remove (package url post-remove-fun)
  "brew remove PACKAGE. URL is there for API compliance."
  (let* ((name (format "*brew remove %s*" package))
         (pkgname (el-get-as-string package))
         (ok   (format "Package %s removed." package))
         (ko   (format "Could not remove package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,(el-get-executable-find "brew")
                      :args ("remove" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-brew-remove-hook 'el-get-dpkg-remove-symlink)

(el-get-register-method :brew
  :install #'el-get-brew-install
  :update #'el-get-brew-update
  :remove #'el-get-brew-remove
  :install-hook 'el-get-brew-install-hook
  :remove-hook 'el-get-brew-remove-hook)

(provide 'el-get-brew)
