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

(require 'el-get-http-zip)

(defun el-get-github-zip-url (package)
  (let* ((source (el-get-package-def package)))
    (or
     ;; Use :url if provided
     (plist-get source :url)
     ;; Else generate URL from username, and reponame
     (let* ((username (el-get-as-string
                       (or (plist-get source :username)
                           (error "Recipe for Github zip package %s needs a username" package))))
            (reponame (el-get-as-string
                       (or (plist-get source :pkgname)
                           package)))
            (branch (or (plist-get source :branch)
                        "master")))
       (format "https://github.com/%s/%s/zipball/%s"
               username reponame branch)))))

(defun el-get-github-zip-install (package url post-install-fun)
  "Clone the given package from Github following the URL."
  (el-get-http-zip-install package
                           (or url (el-get-github-zip-url package))
                           post-install-fun))

(el-get-register-derived-method :github-zip :http-zip
  :install #'el-get-github-zip-install
  :update #'el-get-github-zip-install)

(provide 'el-get-github-zip)
