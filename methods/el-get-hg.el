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

(defcustom el-get-hg-clone-hook nil
  "Hook run after hg clone."
  :group 'el-get
  :type 'hook)

(defun el-get-hg-clone (package url post-install-fun)
  "Clone the given package following the URL."
  (let* ((hg-executable (el-get-executable-find "hg"))
	 (pdir  (el-get-package-directory package))
	 (pname (el-get-as-string package))
	 (name  (format "*hg clone %s*" package))
	 (ok    (format "Package %s installed." package))
	 (ko    (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,hg-executable
		      :args ("clone" ,url ,pname)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-hg-pull (package url post-update-fun)
  "hg pull the package."
  (let* ((hg-executable (el-get-executable-find "hg"))
	 (pdir (el-get-package-directory package))
	 (name (format "*hg pull %s*" package))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,hg-executable
		      :args ("pull" "--update")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))

(defun el-get-hg-compute-checksum (package)
  "Return the hash of the checked-out revision of PACKAGE."
  (with-temp-buffer
    (cd (el-get-package-directory package))
    (let* ((args '("hg" "--debug" "tags"))
           (cmd (mapconcat 'shell-quote-argument args " "))
           (output (shell-command-to-string cmd))
           (hash (and (string-match "^tip[[:space:]]+[0-9]+\\:\\([0-9A-Fa-f]+\\)" output)
                      (match-string 0 output))))
      hash)))

(el-get-register-method :hg
  :install #'el-get-hg-clone
  :update #'el-get-hg-pull
  :remove #'el-get-rmdir
  :install-hook #'el-get-hg-clone-hook
  :compute-checksum #'el-get-hg-compute-checksum)

(provide 'el-get-hg)
