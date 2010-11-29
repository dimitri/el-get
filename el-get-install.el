;;; el-get-install.el --- installer for the lazy
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.9
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs apt-get fink http http-tar
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; bootstrap your el-get installation, the goal is then to use el-get to
;; update el-get.
;;
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.

(let* ((el-get-dir        (expand-file-name "~/.emacs.d/el-get/"))
       (dummy             (unless (file-directory-p el-get-dir)
			    (make-directory el-get-dir t)))
       (package           "el-get")
       (bname             "*el-get bootstrap*")
       (pdir              (concat (file-name-as-directory el-get-dir) package))
       (git               (or (executable-find "git") (error "Unable to find `git'")))
       (url               "git://github.com/dimitri/el-get.git")
       (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
       (default-directory el-get-dir)
       (process-connection-type nil) ; pipe, no pty (--no-progress)
       (status            (call-process git nil bname t "--no-pager" "clone" "-v" url package)))
  (set-window-buffer (selected-window) bname)
  (when (eq 0 status)
    (load (concat (file-name-as-directory pdir) package ".el"))
    ;; (require 'bytecomp)
    (el-get-init "el-get")
    (with-current-buffer bname
      (goto-char (point-max))
      (insert "\nCongrats, el-get is installed and ready to serve!"))))
