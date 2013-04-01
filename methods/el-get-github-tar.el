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

(require 'el-get-http-tar)
(require 'el-get-github)

(defun el-get-github-tar-url (package)
  (let* ((source (el-get-package-def package)))
    (or
     ;; Use :url if provided
     (plist-get source :url)
     ;; Else generate URL from username, and reponame
     (let* ((user-and-repo (el-get-github-parse-user-and-repo package))
            (username (car user-and-repo))
            (reponame (cdr user-and-repo))
            (branch (or (plist-get source :branch)
                        "master")))
       (format "https://github.com/%s/%s/tarball/%s"
               username reponame branch)))))

(defun el-get-github-tar-install (package url post-install-fun)
  "Clone the given package from Github following the URL."
  ;; The recipe must have a `:url' property for
  ;; `el-get-http-tar-install' to work. Also, since github tarballs
  ;; are ".tar.gz", we know what the default tar options should be.
  (let* ((old-pdef (el-get-package-def package))
         (url (or url (el-get-github-tar-url package)))
         (options (or (plist-get old-pdef :options) '("xzf")))
         (new-pdef (append `(:url ,url :options ,options)
                           (el-get-package-def package)))
         (el-get-sources (cons new-pdef el-get-sources)))
    (el-get-http-tar-install package url post-install-fun)))

(el-get-register-derived-method :github-tar :http-tar
  :install #'el-get-github-tar-install
  :update #'el-get-github-tar-install
  :guess-website #'el-get-github-guess-website)

(provide 'el-get-github-tar)
