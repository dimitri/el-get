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

(require 'el-get-http-tar)

(defun el-get-github-tar-url (package)
  (let* ((source (el-get-package-def package)))
    (or
     ;; Use :url if provided
     (plist-get source :url)
     ;; Else generate URL from username, and reponame
     (let* ((username (plist-get source :username))
            (reponame (el-get-as-string
                       (or (plist-get source :pkgname)
                           package)))
            (branch (or (plist-get source :branch)
                        "master")))
       ;; A slash in the repo name means that it is "user/repo"
       (when (string-match-p "/" reponame)
         (let* ((split (split-string reponame "[[:space:]]\\|/" 'omit-nulls)))
           (assert (= (length split) 2) nil
                   "Github pkgname %s must contain only one slash and no spaces" reponame)
           (setq username (first split)
                 reponame (second split))))
       (unless username
         (error "Recipe for Github-tar package %s needs a username" package))
       (format "https://github.com/%s/%s/tarball/%s"
               username reponame branch)))))

(defun el-get-github-tar-install (package url post-install-fun)
  "Clone the given package from Github following the URL."
  (el-get-http-tar-install package
                           (or url (el-get-github-tar-url package))
                           post-install-fun))

(el-get-register-derived-method :github-tar :http-tar
  :install #'el-get-github-tar-install
  :update #'el-get-github-tar-install)

(provide 'el-get-github-tar)
