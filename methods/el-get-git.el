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

(defcustom el-get-git-clone-hook nil
  "Hook run after git clone."
  :group 'el-get
  :type 'hook)

(defun el-get-git-executable ()
  "Return git executable to use, or signal an error when not
found."
  (let ((git-executable (if (and (boundp 'magit-git-executable)
				 (file-executable-p magit-git-executable))
			    magit-git-executable
			  (executable-find "git"))))
    (unless (and git-executable (file-executable-p git-executable))
      (error
       (concat "el-get-git-clone requires `magit-git-executable' to be set, "
	       "or the binary `git' to be found in your PATH")))
    git-executable))

(defun el-get-git-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir   (el-get-package-directory package))
	 (pname  (el-get-as-string package))
	 (name   (format "*git clone %s*" package))
	 (source (el-get-package-def package))
	 (branch (plist-get source :branch))
	 (args   (if branch
		     (list "--no-pager" "clone" "-b" branch url pname)
		   (list "--no-pager" "clone" url pname)))
	 (ok     (format "Package %s installed." package))
	 (ko     (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ,args
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir (el-get-package-directory package))
	 (name (format "*git pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ( "--no-pager" "pull")
		      :message ,ok
		      :error ,ko)
       (:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules"))
     post-update-fun)))

(defun el-get-git-compute-checksum (package)
  "Return the hash of the checked-out revision of PACKAGE."
  (with-temp-buffer
    (cd (el-get-package-directory package))
    (let* ((args '("git" "show-ref" "HEAD"))
           (cmd (mapconcat 'shell-quote-argument args " "))
           (output (shell-command-to-string cmd))
           (hash (and (string-match "^[[:space:]]*\\([^[:space:]]+\\)" output)
                      (match-string 0 output))))
      hash)))

(el-get-register-method
 :git #'el-get-git-clone #'el-get-git-pull #'el-get-rmdir
 #'el-get-git-clone-hook nil #'el-get-git-compute-checksum)

(provide 'el-get-git)
