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

(require 'el-get-http)

(defcustom el-get-emacswiki-base-url
  "http://www.emacswiki.org/emacs/download/%s.el"
  "The base URL where to fetch :emacswiki packages"
  :group 'el-get
  :type 'string)

(defcustom el-get-emacswiki-elisp-index-url
  "http://www.emacswiki.org/cgi-bin/wiki?action=index;match=%5C.(el%7Ctar)(%5C.gz)%3F%24"
  "The emacswiki index URL of elisp pages"
  :group 'el-get
  :type 'string)

(defcustom el-get-emacswiki-elisp-index-base-url
  "http://www.emacswiki.org/emacs/"
  "The emacswiki base URL used in the index"
  :group 'el-get
  :type 'string)

(defun el-get-emacswiki-install (package url post-install-fun)
  "Download a single-file PACKAGE over HTTP from emacswiki."
  (let ((url (or url (format el-get-emacswiki-base-url package))))
    (el-get-http-install package url post-install-fun)))

(defun el-get-emacswiki-retrieve-package-list ()
  "returns a list of (URL . PACKAGE) from emacswiki listing page"
  (with-current-buffer
      (url-retrieve-synchronously el-get-emacswiki-elisp-index-url)
    (goto-char (point-min))
    (re-search-forward "pages found.</h2>" nil 'move)
    (remove-if-not
     (lambda (p) (string-match "el$" (cdr p)))
     (loop
      with offset = (length el-get-emacswiki-elisp-index-base-url)
      ;; <a class="local" href="http://www.emacswiki.org/emacs/thingatpt%2b.el">thingatpt+.el</a>
      while (re-search-forward el-get-emacswiki-elisp-index-base-url nil 'move)
      collect (cons
	       ;; URL
	       (buffer-substring-no-properties
		(- (point) offset)
		(1- (re-search-forward "\"" nil 'move)))
	       ;; PACKAGE name
	       (buffer-substring-no-properties
		(re-search-forward ">" nil 'move)
		(1- (re-search-forward "<" nil 'move))))))))

(defun el-get-emacswiki-build-local-recipes (&optional target-dir)
  "retrieve the index of elisp pages at emacswiki and turn them
into a local recipe file set"
  (let ((target-dir (or target-dir
			(car command-line-args-left)
			el-get-recipe-path-emacswiki)))
    (unless (file-directory-p target-dir) (make-directory target-dir))
    (loop
     for (url . package) in (el-get-emacswiki-retrieve-package-list)
     for recipe = (replace-regexp-in-string "el$" "rcp" package)
     for rfile  = (expand-file-name recipe target-dir)
     unless (file-exists-p rfile)
     do (with-temp-file (expand-file-name rfile target-dir)
	  (message "%s" package)
	  (insert (format "(:name %s :type emacswiki :website \"%s\")"
			  (file-name-sans-extension package) url))))))

(defun el-get-emacswiki-refresh (&optional target-dir)
  "run Emacs -Q in an asynchronous subprocess to get the package
list from emacswiki and build a local recipe directory out of
that"
  (interactive
   (list (let ((dummy (unless (file-directory-p el-get-recipe-path-emacswiki)
			(make-directory el-get-recipe-path-emacswiki))))
	   (read-directory-name "emacswiki recipes go to: "
				el-get-recipe-path-emacswiki))))
  (let* ((name "*el-get-emacswiki*")
	 (dummy (when (get-buffer name) (kill-buffer name)))
	 (args
	  (format "-Q -batch -l %s -f el-get-emacswiki-build-local-recipes %s"
		  (file-name-sans-extension
		   (symbol-file 'el-get-emacswiki-build-local-recipes 'defun))
		  target-dir))
	 (process
	  (apply 'start-process name name el-get-emacs (split-string args))))
    (message "%s %s" el-get-emacs args)
    (set-process-sentinel
     process
     '(lambda (proc event)
	(when (eq (process-status proc) 'exit)
	  (el-get-notify "el-get: EmacsWiki"
			 "EmacsWiki local recipe list refreshed"))))))

(el-get-register-method
 :emacswiki #'el-get-emacswiki-install #'el-get-emacswiki-install #'el-get-rmdir
 #'el-get-http-install-hook)

(provide 'el-get-emacswiki)
