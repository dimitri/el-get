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

(defcustom el-get-pacman-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'directory)

(add-hook 'el-get-pacman-install-hook 'el-get-dpkg-symlink)

(defun el-get-pacman-install (package url post-install-fun)
  "echo $pass | sudo -S pacman install PACKAGE"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (name    (format "*pacman install %s*" package))
         (ok      (format "Package %s installed." package))
         (ko      (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :process-filter ,(function el-get-sudo-password-process-filter)
                      :program ,(el-get-executable-find "sudo")
                      :args ("-S" ,(executable-find "pacman") "--sync" "--noconfirm" "--needed" ,pkgname)
                      :message ,ok
                      :error ,ko
                      :sync t))
     post-install-fun)))

(defun el-get-pacman-remove (package url post-remove-fun)
  "pacman remove PACKAGE, URL is there for API compliance"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (name    (format "*pacman remove %s*" package))
         (ok      (format "Package %s removed." package))
         (ko      (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
                      :buffer-name ,name
                      :process-filter ,(function el-get-sudo-password-process-filter)
                      :program ,(el-get-executable-find "sudo")
                      :args ("-S" ,(executable-find "pacman") "--remove" "--noconfirm" ,pkgname)
                      :message ,ok
                      :error ,ko
                      :sync t))
     post-remove-fun)))

(add-hook 'el-get-pacman-remove-hook 'el-get-dpkg-remove-symlink)

(el-get-register-method :pacman
  :install #'el-get-pacman-install
  :update #'el-get-pacman-install
  :remove #'el-get-pacman-remove
  :install-hook 'el-get-pacman-install-hook
  :remove-hook 'el-get-pacman-remove-hook)

(provide 'el-get-pacman)
