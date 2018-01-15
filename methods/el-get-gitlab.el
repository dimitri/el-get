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

(require 'el-get-core)
(require 'el-get-git)

(defconst el-get-gitlab-url-type-plist
  (list 'http "http://gitlab.com/%USER%/%REPO%.git"
        'https "https://gitlab.com/%USER%/%REPO%.git"
        'git "git://gitlab.com/%USER%/%REPO%.git"
        'ssh "git@gitlab.com:%USER%/%REPO%.git")
  "Plist mapping Github types to their URL format strings.")

(defcustom el-get-gitlab-default-url-type 'https
  "The kind of URL to use for Github repositories.

Choices are `http', `https', `git'. This is effectively the
default `:url-type' for Github recipes that do not specify one.
Individual Github recipes can override this setting by providing
their own `:url-type' property. Note that `ssh' is also an
acceptable value for `:url-type', but you should not set it here
because it will prevent access to any repositories not owned by
you."
  :group 'el-get
  :type '(choice (const :tag "HTTP" http)
                 (const :tag "HTTPS" https)
                 (const :tag "git" git)))

(defun el-get-replace-string (from to str)
  "Replace all instances of FROM with TO in str.

FROM is a literal string, not a regexp."
  (replace-regexp-in-string (regexp-quote from) to str 'fixedcase 'literal))

(defun el-get-gitlab-parse-user-and-repo (package)
  "Returns a cons cell of `(USER . REPO)'."
  (let* ((source (el-get-package-def package))
         (user-slash-repo
          (or (plist-get source :pkgname)
              (error ":pkgname \"username/reponame\" is mandatory for gitlab recipe '%s"
                     package)))
         (user-and-repo (split-string user-slash-repo "/" 'omit-nulls)))
    (assert (= (length user-and-repo) 2) nil
            "Github pkgname %s must be of the form username/reponame"
            user-slash-repo)
    (cons (first user-and-repo) (second user-and-repo))))

(defun el-get-gitlab-url-private (url-type username reponame)
  "Return the url of a particular gitlab project.
URL-TYPE must be a valid property (a symbol) of
`el-get-gitlab-url-type-plist'.
USERNAME and REPONAME are strings."
  (let ((url-format-string
         (or (plist-get el-get-gitlab-url-type-plist url-type)
             (error "Unknown Github repo URL type: %s" url-type))))
    (el-get-replace-string
     "%USER%" username
     (el-get-replace-string "%REPO%" reponame url-format-string))))

(defun el-get-gitlab-url (package)
  (let* ((source (el-get-package-def package))
         (user-and-repo (el-get-gitlab-parse-user-and-repo package))
         (username (car user-and-repo))
         (reponame (cdr user-and-repo))
         (url-type (el-get-as-symbol
                    (or (plist-get source :url-type)
                        el-get-gitlab-default-url-type))))
    (el-get-gitlab-url-private url-type username reponame)))

(defun el-get-gitlab-clone (package _url post-install-fun)
  "Clone the given package from Github following the URL."
  (el-get-git-clone package (el-get-gitlab-url package) post-install-fun))

(defun el-get-gitlab-pull (package _url post-install-fun)
  "Update the given package from Github following the URL."
  (el-get-git-pull package (el-get-gitlab-url package) post-install-fun))

(defun el-get-gitlab-guess-website (package)
  (let* ((user-and-repo (el-get-gitlab-parse-user-and-repo package))
         (username (car user-and-repo))
         (reponame (cdr user-and-repo)))
    (el-get-gitlab-url-private 'https username reponame)))

(el-get-register-derived-method :gitlab :git
  :install #'el-get-gitlab-clone
  :update #'el-get-gitlab-pull
  :guess-website #'el-get-gitlab-guess-website)

(provide 'el-get-gitlab)
