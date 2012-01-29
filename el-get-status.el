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

;;
;; package status --- a plist saved on a file, using symbols
;;
;; it should be possible to use strings instead, but in my tests it failed
;; miserably.
;;

(require 'el-get-core)

(defun el-get-package-symbol (package-name)
  "Returns a symbol :package."
  (if (and (symbolp package-name)
	   (string= (substring (symbol-name package-name) 0 1) ":"))
      package-name
    (intern (format ":%s" package-name))))

(defun el-get-package-name (package-symbol)
  "Returns a string package"
  (if (symbolp package-symbol)
      (cadr (split-string (format "%s" package-symbol) ":"))
    package-symbol))

(defun el-get-read-all-packages-status ()
  "Return the current plist of packages status"
  (when (file-exists-p el-get-status-file)
    (car (with-temp-buffer
	   (insert-file-contents-literally el-get-status-file)
	   (read-from-string (buffer-string))))))

(defun el-get-read-package-status (package)
  "Return the current known status for given package."
  (plist-get (el-get-read-all-packages-status)
	     (el-get-package-symbol package)))

(defun el-get-save-package-status (package status)
  "Save given package status"
  (let ((p (el-get-package-symbol package))
	(s (el-get-read-all-packages-status))
	print-length print-level)
    (with-temp-file el-get-status-file
      (insert
       (format "%S" (if s (plist-put s p status)
		      `(,p ,status)))))))

(defun el-get-list-package-names-with-status (&rest status)
  "Return package names that are currently in given status"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	;; it can happen that (el-get-package-name) returns nil here and
	;; that breaks the completion-read command
	when (and (member s status) (el-get-package-name p))
	collect (el-get-package-name p)))

(defun el-get-read-package-with-status (action &rest status)
  "Read a package name in given status"
  (completing-read (format "%s package: " action)
                   (apply 'el-get-list-package-names-with-status status)))

(defun el-get-count-package-with-status (&rest status)
  "Return how many packages are currently in given status"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	if (member s status) sum 1))

(defun el-get-count-packages-with-status (packages &rest status)
  "Return how many packages are currently in given status in PACKAGES"
  (loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	when (and (member (el-get-as-symbol p) packages)
		  (member s status))
	sum 1))

(defun el-get-package-status (package &optional package-status-plist)
  "Return current status of package from given list"
  (let ((status-plist (or package-status-plist (el-get-read-all-packages-status))))
    (plist-get status-plist (el-get-package-symbol package))))

(defun el-get-extra-packages (&rest packages)
  "Return installed or required packages that are not in given package list"
  (let ((packages
	 ;; &rest could contain both symbols and lists
	 (loop for p in packages
	       when (listp p) append (mapcar 'el-get-as-symbol p)
	       else collect (el-get-as-symbol p))))
    (when packages
	(loop for (p s) on (el-get-read-all-packages-status) by 'cddr
	      for x = (el-get-as-symbol (el-get-package-name p))
	      unless (member x packages)
	      unless (equal s "removed")
	      collect (list x s)))))

(provide 'el-get-status)
