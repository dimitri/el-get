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

(require 'el-get-http-zip)
(require 'el-get-github)

(defun el-get-github-zip-url (package)
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
       (format "https://github.com/%s/%s/zipball/%s"
               username reponame branch)))))

(defun el-get-github-zip-install (package url post-install-fun)
  "Clone the given package from Github following the URL."
  ;; The recipe must have a `:url' property for
  ;; `el-get-http-zip-install' to work.
  (let* ((old-pdef (el-get-package-def package))
         (url (or url (el-get-github-zip-url package)))
         (new-pdef (append `(:url ,url)
                           (el-get-package-def package)))
         (el-get-sources (cons new-pdef el-get-sources)))
    (el-get-http-zip-install package url post-install-fun)))

(el-get-register-derived-method :github-zip :http-zip
  :install #'el-get-github-zip-install
  :update #'el-get-github-zip-install
  :guess-website #'el-get-github-guess-website)

(provide 'el-get-github-zip)
