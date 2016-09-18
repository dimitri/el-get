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
(unless (version< emacs-version "24.4")
  (require 'subr-x))

;;
;; NOTE: this will probably benefit from some autoloading magic, later.
;;
(eval-and-compile
 (add-to-list 'load-path
              (expand-file-name
               "methods"
               (file-name-directory (or load-file-name byte-compile-current-file buffer-file-name)))))

(defun el-get-insecure-check (PACKAGE URL)
  "Raise an error if it's not safe to install PACKAGE from URL.

When `el-get-allow-insecure' is non-nil, check if either of the
following is true and retun nil:

- URL's protocol is in `el-get-secure-protocols'

- URL starts with 'file:///' (without hostname), so it points to the
  local file

- URL starts with username, i.e. 'username@example.com', also known as
  SCP-like syntax

- PACKAGE definition has a non-empty :checksum"
  (assert (stringp URL) nil "URL is nil, can't decide if it's safe to install package '%s'" PACKAGE)
  (let* ((checksum (plist-get (el-get-package-def PACKAGE) :checksum))
         (checksum-empty (or (not (stringp checksum))
                             (if (fboundp 'string-blank-p)
                                 (string-blank-p checksum)
                               (string-match-p "\\`[ \t\n\r]*\\'" checksum)))))
    (when (and (not el-get-allow-insecure)
               (not (string-match "\\`file:///" URL))
               (not (car (member 0 (mapcar (lambda (secure-proto)
                                             (let ((proto-rx (concat "\\`" (regexp-quote secure-proto) "://")))
                                               (string-match-p proto-rx URL))) el-get-secure-protocols))))
               (not (string-match "\\`[-_\.A-Za-z0-9]+@" URL)))
      ;; With not empty :checksum, we can rely on `el-get-post-install' calling
      ;; `el-get-verify-checksum' for security.
      (unless (not checksum-empty)
        (error (concat "Attempting to install PACKAGE "
                       (el-get-as-string PACKAGE)
                       " from insecure URL " URL
                       " without `el-get-allow-insecure'."))))))

(require 'el-get-apt-get)
(require 'el-get-builtin)
(require 'el-get-brew)
(require 'el-get-bzr)
(require 'el-get-cvs)
(require 'el-get-darcs)
(require 'el-get-elpa)
(require 'el-get-emacsmirror)
(require 'el-get-emacswiki)
(require 'el-get-fink)
(require 'el-get-git)
(require 'el-get-github)
(require 'el-get-git-svn)
(require 'el-get-go)
(require 'el-get-hg)
(require 'el-get-http)
(require 'el-get-http-tar)
(require 'el-get-http-zip)
(require 'el-get-github-tar)
(require 'el-get-github-zip)
(require 'el-get-pacman)
(require 'el-get-svn)
(require 'el-get-fossil)

(provide 'el-get-methods)
