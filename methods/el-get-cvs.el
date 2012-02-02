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

(defcustom el-get-cvs-checkout-hook nil
  "Hook run after cvs checkout."
  :group 'el-get
  :type 'hook)

(defun el-get-cvs-checkout (package url post-install-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
	 (source  (el-get-package-def package))
	 (module  (plist-get source :module))
	 (options (plist-get source :options))
	 (pname   (el-get-as-string package))
	 (name    (format "*cvs checkout %s*" package))
	 (ok      (format "Checked out package %s." package))
	 (ko      (format "Could not checkout package %s." package)))

    ;; (message "%S" `(:args ("-d" ,url "checkout" "-d" ,package ,module)))
    ;; (message "el-get-cvs-checkout: %S" (string= options "login"))

    (el-get-start-process-list
     package
     `(,@(when (string= options "login")
	   `((:command-name ,(format "*cvs login %s*" package)
			    :buffer-name ,(format "*cvs login %s*" package)
			    :default-directory ,el-get-dir
			    :process-filter ,(function el-get-sudo-password-process-filter)
			    :program ,cvs-executable
			    :args ("-d" ,url "login")
			    :message "cvs login"
			    :error "Could not login against the cvs server")))

       (:command-name ,name
		      :buffer-name ,name
		      :default-directory ,el-get-dir
		      :program ,cvs-executable
		      :args ("-d" ,url "checkout" "-d" ,pname ,module)
		      :message ,ok
		      :error ,ko))
     post-install-fun)))

(defun el-get-cvs-update (package url post-update-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
	 (pdir (el-get-package-directory package))
	 (name (format "*cvs update %s*" package))
	 (ok   (format "Updated package %s." package))
	 (ko   (format "Could not update package %s." package)))

    (el-get-start-process-list
     package
     `((:command-name ,name
		      :buffer-name ,name
		      :default-directory ,pdir
		      :program ,cvs-executable
		      :args ("update" "-dP")
		      :message ,ok
		      :error ,ko))
     post-update-fun)))

(el-get-register-method :cvs
  :install #'el-get-cvs-checkout
  :update #'el-get-cvs-update
  :remove #'el-get-rmdir
  :install-hook #'el-get-cvs-checkout-hook)

(provide 'el-get-cvs)
