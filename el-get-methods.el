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

;;
;; NOTE: this will probably benefit from some autoloading magic, later.
;;
(add-to-list 'load-path
	     (expand-file-name
	      "methods"
	      (file-name-directory (or load-file-name buffer-file-name))))

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
