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

(defcustom el-get-git-clone-hook nil
  "Hook run after git clone."
  :group 'el-get
  :type 'hook)

(defcustom el-get-git-shallow-clone nil
  "If t, then run git-clone with `--depth 1'."
  :group 'el-get
  :type 'boolean)

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
         (submodule-prop (plist-get source :submodule))
         ;; nosubmodule is true only if :submodules  is explicitly set to nil.
         (explicit-nosubmodule (and (plist-member source :submodule)
                                    (not submodule-prop)))
         (checkout (or (plist-get source :checkout)
		       (plist-get source :checksum)))
         ;; http may a be a dumb server, not supporting shallow clones
         ;; it's not the case of github
         (shallow (unless (and (string-prefix-p "http" url)
                               (not (string-prefix-p "http://github.com" url)))
                    (el-get-plist-get-with-default source :shallow
                                                   el-get-git-shallow-clone)))
	 (clone-args (append '("--no-pager" "clone")
                             (when shallow '("--depth" "1"))
			     (cond
			      ;; With :checkout, the "git checkout"
			      ;; command is a separate step, so don't
			      ;; do it during cloning.
			      (checkout '("--no-checkout"))
			      ;; With a specified branch, check that branch out
			      (branch (list "-b" branch))
			      ;; Otherwise, just checkout the default branch
			      (t nil))
			     (list url pname)))
	 (ok     (format "Package %s installed." package))
	 (ko     (format "Could not install package %s." package)))
    (el-get-start-process-list
     package
     (list
      `(:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,git-executable
		      :args ,clone-args
		      :message ,ok
		      :error ,ko)
      (when checkout
        (list :command-name (format "*git checkout %s*" checkout)
              :buffer-name name
              :default-directory pdir
              :program git-executable
              :args (list "--no-pager" "checkout" checkout)
              :message (format "git checkout %s ok" checkout)
              :error (format "Could not checkout %s for package %s" checkout package)))
      (unless explicit-nosubmodule
        (list :command-name "*git submodule update*"
              :buffer-name name
              :default-directory pdir
              :program git-executable
              :args (list "--no-pager" "submodule" "update" "--init" "--recursive")
              :message "git submodule update ok"
              :error "Could not update git submodules")))
     post-install-fun)))

(defun el-get-git-pull (package url post-update-fun)
  "git pull the package."
  (let* ((git-executable (el-get-executable-find "git"))
	 (pdir (el-get-package-directory package))
	 (name (format "*git pull %s*" package))
	 (source (el-get-package-def package))
         (submodule-prop (plist-get source :submodule))
         ;; nosubmodule is true only if :submodules  is explicitly set to nil.
         (explicit-nosubmodule (and (plist-member source :submodule)
                                     (not submodule-prop)))
         (checkout (or (plist-get source :checkout)
		       (plist-get source :checksum)))
	 ;; When dealing with a specific checkout, we cannot use
	 ;; "pull", but must instead use "fetch" and then "checkout".
	 (pull-args (list "--no-pager" (if checkout "fetch" "pull")))
	 (ok   (format "Pulled package %s." package))
	 (ko   (format "Could not update package %s." package)))
    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ,pull-args
		      :message ,ok
		      :error ,ko)
       ,(when checkout
	  (list :command-name (format "*git checkout %s*" checkout)
		:buffer-name name
		:default-directory pdir
		:program git-executable
		:args (list "--no-pager" "checkout" checkout)
		:message (format "git checkout %s ok" checkout)
		:error (format "Could not checkout %s for package %s" checkout package)))
       ,(unless explicit-nosubmodule
	  `(:command-name "*git submodule update*"
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,git-executable
		      :args ("--no-pager" "submodule" "update" "--init" "--recursive")
		      :message "git submodule update ok"
		      :error "Could not update git submodules")))
     post-update-fun)))

(defun el-get-git-compute-checksum (package)
  "Return the hash of the checked-out revision of PACKAGE."
  (with-temp-buffer
    (cd (el-get-package-directory package))
    ;; We cannot simply check the recipe for `:type git' because it
    ;; could also be github, emacsmirror, or any other unknown git-ish
    ;; type. Instead, we check for the existence of a ".git" directory
    ;; in the package directory. A better approach might be to call
    ;; "git status" and check that it returns success.
    (assert (file-directory-p ".git") nil
            "Package %s is not a git package" package)
    (let* ((git-executable (el-get-executable-find "git"))
           (args (list git-executable "log" "--pretty=format:%H" "-n1"))
           (cmd (mapconcat 'shell-quote-argument args " ")))
      (shell-command-to-string cmd))))

(el-get-register-method :git
  :install #'el-get-git-clone
  :update #'el-get-git-pull
  :remove #'el-get-rmdir
  :install-hook #'el-get-git-clone-hook
  :compute-checksum #'el-get-git-compute-checksum)

(provide 'el-get-git)
