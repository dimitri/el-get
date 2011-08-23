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

(let ((el-get-root
       (file-name-as-directory
	(concat (file-name-as-directory user-emacs-directory) "el-get"))))

  (when (file-directory-p el-get-root)
    (add-to-list 'load-path el-get-root))

  ;; try to require el-get, failure means we have to install it
  (unless (require 'el-get nil t)
    (unless (file-directory-p el-get-root)
      (make-directory el-get-root t))

    (let* ((package   "el-get")
	   (buf       (switch-to-buffer "*el-get bootstrap*"))
	   (pdir      (file-name-as-directory (concat el-get-root package)))
	   (git       (or (executable-find "git")
			  (error "Unable to find `git'")))
	   (url       (if (bound-and-true-p el-get-git-install-url)
			  el-get-git-install-url
			"http://github.com/dimitri/el-get.git"))
	   (default-directory el-get-root)
	   (process-connection-type nil)   ; pipe, no pty (--no-progress)
	   (el-get-default-process-sync t) ; force sync operations for installer
	   (el-get-verbose t)		   ; let's see it all

	   ;; First clone el-get
	   (status
	    (call-process
	     git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))

      (unless (zerop status)
	(error "Couldn't clone el-get from the Git repository: %s" url))

      ;; switch branch if we have to
      (let* ((branch  (plist-get (with-temp-buffer
				   (insert-file-contents-literally
				    (expand-file-name "recipes/el-get.el" pdir))
				   (read (current-buffer)))
				 :branch))
	     (branch (when branch (concat "origin/" branch)))
	     (default-directory pdir)
	     (bstatus
	      (when branch
		(call-process git nil `(,buf t) t "checkout" "-t" branch))))

	(when (and branch (not (zerop bstatus)))
	  (error "Couldn't `git checkout -t %s`" branch)))

      (load (concat pdir package ".el"))
      (el-get-post-install "el-get")
      (with-current-buffer buf
	(goto-char (point-max))
	(insert "\nCongrats, el-get is installed and ready to serve!")))))

