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

(defcustom el-get-cvs-checkout-hook nil
  "Hook run after cvs checkout."
  :group 'el-get
  :type 'hook)

(defcustom el-get-cvs-http-proxy-url-use-user-name nil
  "Use user-name and password for CVS proxy.

Some implementations of CVS do not support user and password
proxy properties.

Enable this if you want el-get to honor these settings"
  :type 'boolean
  :group 'el-get)

(defun el-get-parse-proxy ()
  "Parse HTTP_PROXY or use `url-proxy-services'"
  (let ((proxy (or (getenv "HTTP_PROXY")
		   (if (and (featurep 'url-vars)
			    (assoc "http" url-proxy-services))
		       (cadr (assoc "http" url-proxy-services))
		     nil)))
        port ret
        (user "")
        (password "")
        (url-proxy url))
    (if (or (not proxy) (string= proxy ""))
        (symbol-value 'ret)
      (when (string-match "^https?://" proxy)
        (setq proxy (replace-match "" nil nil proxy)))
      (when (string-match "^\\(.*\\)@" proxy)
        (setq user (match-string 1 proxy))
        (setq proxy (replace-match "" nil nil proxy))
        (when (string-match "^\\([^:]*\\):\\(.*\\)$" user)
          (setq password (match-string 2 user))
          (setq user (match-string 1 user))))
      (when (string-match "^\\(.*\\):\\([0-9]+\\)$" proxy)
        (setq port (match-string 2 proxy))
        (setq proxy (match-string 1 proxy)))
      (setq ret `(:user ,user
			:password ,password
			:proxy ,proxy
			:port ,port))
      (symbol-value 'ret))))

(defun el-get-cvs-checkout-proxy-url (url)
  "Checkout proxy URL"
  (let ((proxy (el-get-parse-proxy))
	(cvs-proxy "")
	tmp
	(ret url))
    (when proxy
      (setq cvs-proxy (plist-get proxy ':proxy))
      (unless (string= "" cvs-proxy)
	(setq cvs-proxy (concat ";proxy=" cvs-proxy))
	(setq tmp (plist-get proxy ':port))
	(unless (string= "" tmp)
	  (setq cvs-proxy (concat cvs-proxy ";proxyport=" tmp)))
        (when el-get-cvs-http-proxy-url-use-user-name
	  (setq tmp (plist-get proxy ':user))
	  (unless (string= "" tmp)
	    (setq cvs-proxy (concat cvs-proxy ";proxyuser=" tmp)))
	  (setq tmp (plist-get proxy ':password))
	  (unless (string= "" tmp)
	    (setq cvs-proxy (concat cvs-proxy ";proxypassword=" tmp))))
	(when (string-match ":pserver:" ret)
	  (setq ret (replace-match (concat ":pserver" cvs-proxy ":") t t ret)))))
    (symbol-value 'ret)))

(defun el-get-cvs-checkout (package urlp post-install-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
         (url (el-get-cvs-checkout-proxy-url urlp))
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

(defun el-get-cvs-update (package urlp post-update-fun)
  "cvs checkout the package."
  (let* ((cvs-executable (el-get-executable-find "cvs"))
         (url (el-get-cvs-checkout-proxy-url urlp))
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
