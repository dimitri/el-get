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

(defcustom el-get-fink (executable-find "fink")
  "The fink executable."
  :group 'el-get
  :type 'directory)

(defcustom el-get-fink-base "/sw/share/doc"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'file)

(defcustom el-get-fink-install-hook nil
  "Hook run after fink install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-fink-remove-hook nil
  "Hook run after fink remove."
  :group 'el-get
  :type 'hook)

(defun el-get-fink-install (package url post-install-fun)
  "sudo -S fink install PACKAGE"
  (let* ((name (format "*fink install %s*" package))
         (source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (ok   (format "Package %s installed." package))
         (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :process-filter ,(function el-get-sudo-password-process-filter)
                      :program ,(el-get-executable-find "sudo")
                      :args ("-S" ,(executable-find "fink") "install" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-install-fun)))

(add-hook 'el-get-fink-install-hook 'el-get-dpkg-symlink)

(defun el-get-fink-remove (package url post-remove-fun)
  "apt-get remove PACKAGE. URL is there for API compliance."
  (let* ((name (format "*fink remove %s*" package))
         (source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (ok   (format "Package %s removed." package))
         (ko   (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :process-filter ,(function el-get-sudo-password-process-filter)
                      :program ,(el-get-executable-find "sudo")
                      :args ("-S" ,(executable-find "fink") "-y" "remove" ,pkgname)
                      :message ,ok
                      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-fink-remove-hook 'el-get-dpkg-remove-symlink)

(el-get-register-method :fink
  :install #'el-get-fink-install
  :update #'el-get-fink-install
  :remove #'el-get-fink-remove
  :install-hook 'el-get-fink-install-hook
  :remove-hook 'el-get-fink-remove-hook)

(provide 'el-get-fink)
