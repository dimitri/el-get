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

(require 'el-get-core)
(require 'el-get-git)

(defconst el-get-github-url-type-plist
  (list 'http "http://github.com/%USER%/%REPO%.git"
        'https "https://github.com/%USER%/%REPO%.git"
        'git "git://github.com/%USER%/%REPO%.git"
        'ssh "git@github.com:%USER%/%REPO%.git")
  "Plist mapping Github types to their URL format strings.")

(defun el-get-github-url-format (url-type)
  (or (plist-get el-get-github-url-type-plist
                 url-type)
      (error "Unknown Github repo URL type: %s" url-type)))

(defcustom el-get-github-default-url-type 'http
  "The kind of URL to use for Github repositories.

Choices are `http', `https', `git'. This is effectively the
default `:url-type' for Github recipes that do not specify one.
Individual Github recipes can override this setting by providing
their own `:url-type' property. Note that `ssh' is also an
acceptable value for `:url-type', but you should not set it here
because it will prevent access to any repositories not owned by
you.

You can also supply a custom format string, which must contain
the tokens \"%USER%\" and \"%REPO%\" at the locations where the
username and repo name should be substituted in."
  :group 'el-get
  :type '(choice (const :tag "HTTP" http)
                 (const :tag "HTTPS" https)
                 (const :tag "git" git)
                 (string :tag "Custom string"
                         :match (lambda (widget value)
                                  (and (string-match-p (regexp-quote "%USER%")
                                                       value)
                                       (string-match-p (regexp-quote "%REPO%")
                                                       value))))))

(defun el-get-replace-string (from to str)
  "Replace all instances of FROM with TO in str.

FROM is a literal string, not a regexp."
  (replace-regexp-in-string (regexp-quote from) to str 'fixedcase 'literal))

(defun el-get-github-parse-user-and-repo (package)
  "Returns a cons cell of `(USER . REPO)'."
  (let* ((source (el-get-package-def package))
         (type (el-get-package-method package))
         (username (plist-get source :username))
         (reponame (el-get-as-string
                    (or (plist-get source :pkgname)
                        package))))
    (when (string-match-p "/" reponame)
      (let* ((split (split-string reponame "[[:space:]]\\|/" 'omit-nulls)))
        (assert (= (length split) 2) nil
                "Github pkgname %s must contain only one slash and no spaces" reponame)
        (setq username (first split)
              reponame (second split))))
    (unless username
      (error "Recipe for %s package %s needs a username" type package))
    (cons username reponame)))

(defun el-get-github-url (package)
  (let* ((source (el-get-package-def package)))
    (or
     ;; Use :url if provided
     (plist-get source :url)
     ;; Else generate URL from username, reponame, and url-type
     (let* ((user-and-repo (el-get-github-parse-user-and-repo package))
            (username (car user-and-repo))
            (reponame (cdr user-and-repo))
            (url-type (el-get-as-symbol
                       (or (plist-get source :url-type)
                           el-get-github-default-url-type)))
            (url-format-string
             (if (stringp url-type)
                 url-type
               (or (plist-get el-get-github-url-type-plist
                              url-type)
                   (error "Unknown Github URL type: %s" url-type)))))
       (el-get-replace-string
        "%USER%" username
        (el-get-replace-string
         "%REPO%" reponame
         url-format-string))))))

(defun el-get-github-clone (package url post-install-fun)
  "Clone the given package from Github following the URL."
  (el-get-git-clone package
                    (or url (el-get-github-url package))
                    post-install-fun))

(defun el-get-guess-github-website (url)
  "If a package's URL is on Github, return the project's Github URL."
  (when (and url (string-match "github\\.com/" url))
    (replace-regexp-in-string "\\.git$" ""
                              (replace-regexp-in-string "\\(git\\|https\\)://" "http://" url))))

(defun el-get-github-guess-website (package)
  (let* ((user-and-repo (el-get-github-parse-user-and-repo package))
         (username (car user-and-repo))
         (reponame (cdr user-and-repo))
         (url-format-string "https://github.com/%USER%/%REPO%"))
    (el-get-replace-string
     "%USER%" username
     (el-get-replace-string
      "%REPO%" reponame
      url-format-string))))

(el-get-register-derived-method :github :git
  :install #'el-get-github-clone
  :guess-website #'el-get-github-guess-website)

(provide 'el-get-github)
