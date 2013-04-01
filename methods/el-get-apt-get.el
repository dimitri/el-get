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

(defcustom el-get-apt-get (executable-find "apt-get")
  "The apt-get executable."
  :group 'el-get
  :type 'file)

(defcustom el-get-apt-get-base "/usr/share/emacs/site-lisp"
  "Where to link the el-get symlink to, /<package> will get appended."
  :group 'el-get
  :type 'directory)

(defcustom el-get-apt-get-install-hook nil
  "Hook run after apt-get install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-apt-get-remove-hook nil
  "Hook run after apt-get remove."
  :group 'el-get
  :type 'hook)

;;
;; utilities for both apt-get and fink support (dpkg based)
;;
(defun el-get-dpkg-package-status (package)
  "Return the package status from dpkg --get-selections."
  (substring
   (shell-command-to-string
    (format
     "dpkg -l %s| awk '/^ii/ && $2 = \"%s\" {print \"ok\"}'" package package)) 0 -1))

;;
;; those functions are meant as hooks at install and remove, and they will
;; get the global value of package, which has been set before calling
;; run-hooks.
;;
(defun el-get-dpkg-symlink (package)
  "ln -s /usr/share/emacs/site-lisp/package ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package))
	 (method  (el-get-package-method package))
	 (pname   (el-get-as-string package))
	 (basedir (cond ((eq method 'apt-get) el-get-apt-get-base)
                        ((eq method 'brew)    el-get-brew-base)
			((eq method 'fink)    el-get-fink-base)
			((eq method 'pacman)  el-get-pacman-base)))
	 (debdir  (concat (file-name-as-directory basedir) pname)))
    (unless (file-directory-p pdir)
      (shell-command
       (concat "cd " el-get-dir " && ln -s " debdir  " " pname)))))

(defun el-get-dpkg-remove-symlink (package)
  "rm -f ~/.emacs.d/el-get/package"
  (let* ((pdir    (el-get-package-directory package)))
    (when (file-symlink-p pdir)
      (let ((command (concat "cd " el-get-dir " && rm -f " pname)))
        (message command)
        (shell-command command)))))


;;
;; apt-get support
;;
(add-hook 'el-get-apt-get-install-hook 'el-get-dpkg-symlink)

(defvar el-get-sudo-password-process-filter-pos)

(defun el-get-sudo-password-process-filter (proc string)
  "Filter function that fills the process buffer's and matches a
password prompt."
  (unless (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      ;; arrange to remember already seen content
      (unless (boundp 'el-get-sudo-password-process-filter-pos)
	(make-local-variable 'el-get-sudo-password-process-filter-pos)
	(setq el-get-sudo-password-process-filter-pos (point-min)))

      ;; first, check about passwords
      (save-excursion
	(goto-char (point-max))
	(insert string)
	;; redirect the subprocess sudo prompt to the user face, and answer it
	(goto-char el-get-sudo-password-process-filter-pos)
	(while (re-search-forward "password" nil t)
	  (let* ((prompt (thing-at-point 'line))
		 (pass   (read-passwd prompt)))
	    (process-send-string proc (concat pass "\n")))))

      ;; second, check about "Do you want to continue [Y/n]?" prompts
      (save-excursion
	(while (re-search-forward "Do you want to continue" nil t)
	  (set-window-buffer (selected-window) (process-buffer proc))
	  (let* ((prompt (thing-at-point 'line))
		 (cont   (yes-or-no-p (concat prompt " "))))
	    (process-send-string proc (concat (if cont "y" "n") "\n")))))

      (setq el-get-sudo-password-process-filter-pos (point-max)))))

(defun el-get-apt-get-install (package url post-install-fun)
  "echo $pass | sudo -S apt-get install PACKAGE"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (name (format "*apt-get install %s*" package))
	 (ok   (format "Package %s installed." package))
	 (ko   (format "Could not install package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,el-get-apt-get "install" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-apt-get-remove (package url post-remove-fun)
  "apt-get remove PACKAGE, URL is there for API compliance"
  (let* ((source  (el-get-package-def package))
         (pkgname (or (plist-get source :pkgname) (el-get-as-string package)))
         (name (format "*apt-get remove %s*" package))
	 (ok   (format "Package %s removed." package))
	 (ko   (format "Could not remove package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :process-filter ,(function el-get-sudo-password-process-filter)
		      :program ,(executable-find "sudo")
		      :args ("-S" ,el-get-apt-get "remove" "-y" ,pkgname)
		      :message ,ok
		      :error ,ko))
     post-remove-fun)))

(add-hook 'el-get-apt-get-remove-hook 'el-get-dpkg-remove-symlink)

(el-get-register-method :apt-get
  :install #'el-get-apt-get-install
  :update #'el-get-apt-get-install
  :remove #'el-get-apt-get-remove
  :install-hook #'el-get-apt-get-install-hook
  :remove-hook #'el-get-apt-get-remove-hook)

(provide 'el-get-apt-get)
