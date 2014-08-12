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

(require 'el-get-git)

(defcustom el-get-git-svn-checkout-hook nil
  "Hook run after git-svn checkout."
  :group 'el-get
  :type 'hook)

(defun el-get-git-svn-clone (package url post-install-fun)
  "Clone the given svn PACKAGE following the URL using git."
  (let* ((git-executable (el-get-executable-find "git"))
         (pdir   (el-get-package-directory package))
         (pname  (el-get-as-string package))
         (name   (format "*git svn clone %s*" package))
         (source (el-get-package-def package))
         (checkout (or (plist-get source :checkout)
                       (plist-get source :checksum)))
         (ok    (format "Package %s installed." package))
         (ko    (format "Could not install package %s." package)))
    ;; TODO: not sure if it's possible for svn:// URLs to use TLS?
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     (list
      `(:command-name ,name
                      :buffer-name ,name
                      :default-directory ,el-get-dir
                      :program ,git-executable
                      :args ( "--no-pager" "svn" "clone" ,url ,pname)
                      :message ,ok
                      :error ,ko)
      (when checkout
        (list :command-name (format "*git checkout %s*" checkout)
              :buffer-name name
              :default-directory pdir
              :program git-executable
              :args (list "--no-pager" "checkout" checkout)
              :message (format "git checkout %s ok" checkout)
              :error (format "Could not checkout %s for package %s" checkout package))))
     post-install-fun)))

(defun el-get-git-svn-update (package url post-update-fun)
  "Update PACKAGE using git-svn. URL is given for compatibility reasons."
  (let ((git-executable (el-get-executable-find "git"))
        (pdir   (el-get-package-directory package))
        (f-name (format "*git svn fetch %s*" package))
        (f-ok   (format "Fetched package %s." package))
        (f-ko   (format "Could not fetch package %s." package))
        (r-name (format "*git svn rebase %s*" package))
        (r-ok   (format "Rebased package %s." package))
        (r-ko   (format "Could not rebase package %s." package)))
    (el-get-insecure-check package url)

    (el-get-start-process-list
     package
     `((:command-name ,f-name
                      :buffer-name ,f-name
                      :default-directory ,pdir
                      :program ,git-executable
                      :args ("--no-pager" "svn" "fetch")
                      :message ,f-ok
                      :error ,f-ko)

       (:command-name ,r-name
                      :buffer-name ,r-name
                      :default-directory ,pdir
                      :program ,git-executable
                      :args ("--no-pager" "svn" "rebase")
                      :message ,r-ok
                      :error ,r-ko))
     post-update-fun)))

(el-get-register-derived-method :git-svn :git
  :install #'el-get-git-svn-clone
  :update #'el-get-git-svn-update
  :install-hook 'el-get-git-svn-clone-hook)

(provide 'el-get-git-svn)
