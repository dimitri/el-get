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

(if (require 'el-get nil t)
    (message "el-get is already installed, try M-x el-get-update")
  (let* ((el-get-root
	  (file-name-as-directory
	   (concat (file-name-as-directory user-emacs-directory) "el-get")))
	 (dummy             (unless (file-directory-p el-get-root)
			      (make-directory el-get-root t)))
	 (package           "el-get")
	 (buf               (switch-to-buffer "*el-get bootstrap*"))
	 (pdir              (file-name-as-directory (concat el-get-root package)))
	 (git               (or (executable-find "git")
				(error "Unable to find `git'")))
	 (url               "git://github.com/dimitri/el-get.git")
	 (default-directory el-get-root)
	 (process-connection-type nil) ; pipe, no pty (--no-progress)

	 ;; First clone el-get
	 (status
	  (call-process git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))

    (unless (zerop status)
      (error "Couldn't get el-get from the Git repository"))

    (load (concat pdir package ".el"))
    (el-get-post-install "el-get" 'noerror)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\nCongrats, el-get is installed and ready to serve!"))))
