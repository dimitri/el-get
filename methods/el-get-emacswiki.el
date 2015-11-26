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

(require 'el-get-recipes)
(require 'el-get-notify)
(require 'el-get-http)

(defcustom el-get-emacswiki-base-url
  "https://raw.github.com/emacsmirror/emacswiki.org/master/"
  "The base URL where to fetch :emacswiki packages"
  :group 'el-get
  :type '(radio
          (const :tag "Github mirror (recommended): https://raw.github.com/emacsmirror/emacswiki.org/master/"
                 "https://raw.github.com/emacsmirror/emacswiki.org/master/")
          (const :tag "Main EmacsWiki site: http://www.emacswiki.org/emacs/download/"
                 "http://www.emacswiki.org/emacs/download/")
          (string :tag "Other URL")))

(defcustom el-get-emacswiki-elisp-file-list-url
  "http://www.emacswiki.org/emacs?action=elisp"
  "The emacswiki URL where to fetch a list of elisp files with descriptions.

We get back list of filename space first line, and in general
that matches the following pattern:

filename.el ;;; filename.el --- description"
  :group 'el-get
  :type 'string)

(defun el-get-emacswiki-install (package url post-install-fun)
  "Download a single-file PACKAGE over HTTP from emacswiki."
  (let ((url (or url (format "%s%s.el" el-get-emacswiki-base-url package))))
    (el-get-insecure-check package "http://insecure") ; insecure even over HTTPS
    (el-get-http-install package url post-install-fun)))

(defun el-get-emacswiki-compute-checksum (package)
  "Download a single-file PACKAGE over HTTP from emacswiki."
  (let ((url (or (plist-get (el-get-package-def package) :url)
                 (format "%s%s.el" el-get-emacswiki-base-url package))))
    (el-get-http-compute-checksum package url)))

(defun el-get-emacswiki-guess-website (package)
  (format "%s%s.el" el-get-emacswiki-base-url package))

(el-get-register-derived-method :emacswiki :http
  :install #'el-get-emacswiki-install
  :update #'el-get-emacswiki-install
  :compute-checksum #'el-get-emacswiki-compute-checksum
  :guess-website #'el-get-emacswiki-guess-website)

;;;
;;; Functions to maintain a local recipe list from EmacsWiki
;;;
(defun el-get-emacswiki-retrieve-package-list ()
  "return a list of (URL PACKAGE DESCRIPTION) from emacswiki"
  (with-current-buffer
      (url-retrieve-synchronously el-get-emacswiki-elisp-file-list-url)
    ;; skip HTTP headers
    (goto-char (point-min))
    (unless (looking-at-p "^HTTP/[0-9]\\.[0-9] 2..")
      (error "Failed to retrieve emacswiki package list: %s."
             (buffer-substring (point) (line-end-position))))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (loop
     with wiki-regexp =
     (concat "^\\([^[:space:]]+\\.el\\)" ; filename
             "\\(?:" ; optional separators
             " +\\(?:;;;+\\)? *\\(?:\\1\\)? *\\(?:---\\)? *\\(?:[-;]\\{4,\\}\\)?"
             "\\(.*\\)\\)?$") ; description
     while (not (eobp))
     when (re-search-forward wiki-regexp (line-end-position) 'noerror)
     collect (let* ((filename (match-string 1))
                    (description (or (match-string 2) ""))
                    (url (format "%s%s" el-get-emacswiki-base-url filename)))
               (list url filename
                     (replace-regexp-in-string "[[:space:]]*-[*]-.*\\'\\|;;;+\\'"
                                               "" description)))
     do (forward-line))))

(defun el-get-emacswiki-build-local-recipes (&optional target-dir)
  "retrieve the index of elisp pages at emacswiki and turn them
into a local recipe file set"
  (interactive)
  (let ((target-dir (or target-dir
                        (car command-line-args-left)
                        el-get-recipe-path-emacswiki))
        (coding-system-for-write 'utf-8))
    (unless (file-directory-p target-dir) (make-directory target-dir 'recursive))
    (loop
     with wiki-list = (el-get-emacswiki-retrieve-package-list)
     with progress = (make-progress-reporter "Generating Emacswiki recipes"
                                             0 (length wiki-list))
     for (url package description) in wiki-list
     for recipe = (replace-regexp-in-string "el$" "rcp" package)
     for rfile  = (expand-file-name recipe target-dir)
     for recipe-num from 0
     unless (file-exists-p rfile)
     do (with-temp-file (expand-file-name rfile target-dir)
          (progress-reporter-update progress recipe-num)
          (insert
           (format
            "(:name %s\n:auto-generated t\n:type emacswiki\n:description %S\n:website %S)\n"
            (file-name-sans-extension package) description url)))
     finally (progress-reporter-done progress))))

;;;###autoload
(defun el-get-emacswiki-refresh (&optional target-dir in-process)
  "Generate recipes for all lisp files on Emacswiki.

By default, this is done in a separate process so that you can
continue to work while the recipes are being updated. If this
fails, you can force the update to be done in-process by running
this with a prefix arg (noninteractively: set optional arg
`in-process' non-nil)."
  (interactive
   (list (let ((dummy (unless (file-directory-p el-get-recipe-path-emacswiki)
                        (make-directory el-get-recipe-path-emacswiki))))
           (read-directory-name "emacswiki recipes go to: "
                                el-get-recipe-path-emacswiki))
         current-prefix-arg))
  (if in-process
      (progn
        (el-get-emacswiki-build-local-recipes target-dir)
        (el-get-notify "el-get: EmacsWiki"
                       "EmacsWiki local recipe list refreshed"))
    (let* ((name "*el-get-emacswiki*")
           (dummy (when (get-buffer name) (kill-buffer name)))
           (args
            (format
             "-Q -batch -L %s -L %s -l %s -f el-get-emacswiki-build-local-recipes %s"
             (el-get-package-directory 'el-get)
             (expand-file-name "methods" (el-get-package-directory 'el-get))
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
                           "EmacsWiki local recipe list refreshed")))))))

(provide 'el-get-emacswiki)
