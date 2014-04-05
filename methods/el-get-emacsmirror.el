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
(require 'el-get-recipes)
(require 'el-get-git)
(require 'el-get-github)

(defun el-get-emacsmirror-get-github-source (package)
  "Return a github-type source equivalent to emacsmirror PACKAGE."
  (assert (equal (el-get-package-type package) 'emacsmirror) nil
          "Need an emacsmirror package")
  (append `(:type github
                  :pkgname ,(format "emacsmirror/%s" package))
          (el-get-package-def package)))

;;
;; emacsmirror support
;;
(defun el-get-emacsmirror-clone (package url post-install-fun)
  ;; Override the package def with an equivalent github-type package,
  ;; then run the github method.
  (let* ((package-github-source
          (el-get-emacsmirror-get-github-source package))
         (el-get-sources (cons package-github-source el-get-sources)))
    (el-get-github-clone package url post-install-fun)))

(defun el-get-emacsmirror-guess-website (package)
  (let* ((package-github-source
          (el-get-emacsmirror-get-github-source package))
         (el-get-sources (cons package-github-source el-get-sources)))
    (el-get-github-guess-website package)))

(el-get-register-derived-method :emacsmirror :github
  :install #'el-get-emacsmirror-clone
  :guess-website #'el-get-emacsmirror-guess-website)

(provide 'el-get-emacsmirror)
