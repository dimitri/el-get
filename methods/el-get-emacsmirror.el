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
(require 'el-get-github)

;;
;; emacsmirror support
;;
(defun el-get-emacsmirror-clone (package url post-install-fun)
  ;; Override the package def with an equivalent github-type package,
  ;; then run the github method.
  (let* ((package-github-source (append '(:type github :username "emacsmirror")
                                (el-get-package-def package)))
         (el-get-sources (cons package-github-source el-get-sources)))
    (el-get-github-clone package url post-install-fun)))

(el-get-register-derived-method :emacsmirror :github
  :install #'el-get-emacsmirror-clone)

(provide 'el-get-emacsmirror)
