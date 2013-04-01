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
         (branch   (plist-get source :branch))
         (checkout (or (plist-get source :checkout)
                       (plist-get source :checksum)
                       (plist-get source :branch)))
         (clone-args (append '("clone")
                             (when checkout (list "--updaterev" checkout))
                             (list url pname)))
	 (ok    (format "Package %s installed." package))
	 (ko    (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,hg-executable
		      :args ,clone-args
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-hg-pull (package url post-update-fun)
  "hg pull the package."
  (let* ((hg-executable (el-get-executable-find "hg"))
	 (pdir (el-get-package-directory package))
	 (name (format "*hg pull %s*" package))
         ;; Don't put :branch here, because by default `hg update'
         ;; updates to tip of current branch.
         (checkout (or (plist-get source :checkout)
                       (plist-get source :checksum)))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,hg-executable
		      :args ("pull")
		      :message ,ok
		      :error ,ko)
       ,(list :command-name (format "*hg checkout %s*" checkout)
              :buffer-name name
              :default-directory pdir
              :program hg-executable
              :args (append '("update")
                            (when checkout (list "--rev" checkout)))
              :message (format "hg checkout %s ok" checkout)
              :error (format "Could not checkout %s for package %s"
                             checkout package)))
     post-update-fun)))

(defun el-get-hg-compute-checksum (package)
  "Return the hash of the checked-out revision of PACKAGE."
  (with-temp-buffer
    (cd (el-get-package-directory package))
    (let* ((args '("hg" "log" "--rev" "." "--template" "{node}"))
           (cmd (mapconcat 'shell-quote-argument args " "))
           (hash (shell-command-to-string cmd)))
      hash)))

(el-get-register-method :hg
  :install #'el-get-hg-clone
  :update #'el-get-hg-pull
  :remove #'el-get-rmdir
  :install-hook #'el-get-hg-clone-hook
  :compute-checksum #'el-get-hg-compute-checksum)

(provide 'el-get-hg)
