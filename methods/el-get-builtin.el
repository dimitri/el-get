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

(defcustom el-get-builtin-install-hook nil
  "Hook run after 'installing' a builtin package."
  :group 'el-get
  :type 'hook)

(defun el-get-builtin-install (package url post-install-fun)
  (let ((pdir (el-get-package-directory package)))
    (unless (file-directory-p pdir)
      (make-directory pdir))
    (funcall post-install-fun package)))

(el-get-register-method :builtin
  :install #'el-get-builtin-install
  :update #'el-get-builtin-install
  :remove #'el-get-rmdir
  :install-hook 'el-get-builtin-install-hook)

(el-get-register-method-alias :no-op :builtin)

(provide 'el-get-builtin)
