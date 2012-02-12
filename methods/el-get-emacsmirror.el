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
;;     Please see the README.asciidoc file from the same distribution

(require 'el-get-git)

(defcustom el-get-emacsmirror-base-url
  "http://github.com/emacsmirror/%s.git"
  "The base URL where to fetch :emacsmirror packages.  Consider using
\"git://github.com/emacsmirror/%s.git\"."
  :group 'el-get
  :type '(choice (const "http://github.com/emacsmirror/%s.git")
                 (const "https://github.com/emacsmirror/%s.git")
                 (const "git://github.com/emacsmirror/%s.git")
                 string))

;;
;; emacsmirror support
;;
(defun el-get-emacsmirror-clone (package url post-install-fun)
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
	 (url     (or url (format el-get-emacsmirror-base-url pkgname))))
    (el-get-git-clone package url post-install-fun)))

(el-get-register-derived-method :emacsmirror :git
  :install #'el-get-emacsmirror-clone)

(provide 'el-get-emacsmirror)
