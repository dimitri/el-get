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

(defcustom el-get-http-install-hook nil
  "Hook run after http retrieve."
  :group 'el-get
  :type 'hook)

(defun el-get-filename-from-url (url)
  "return a suitable filename from given url

Test url: http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=ack.el"
  (replace-regexp-in-string "[^a-zA-Z0-9-_\.\+]" "_"
			    (file-name-nondirectory url)))

(defun el-get-http-retrieve-callback (status package post-install-fun &optional dest sources)
  "Callback function for `url-retrieve', store the emacs lisp file for the package."
  (let* ((pdir   (el-get-package-directory package))
	 (dest   (or dest (concat (file-name-as-directory pdir) package ".el")))
	 (part   (concat dest ".part"))
	 (el-get-sources (if sources sources el-get-sources))
	 (buffer-file-coding-system 'no-conversion)
	 (require-final-newline nil))
    ;; prune HTTP headers before save
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (forward-char)
    (delete-region (point-min) (point))
    (write-file part)
    (when (file-exists-p dest)
      (delete-file dest))
    (rename-file part dest)
    (message "Wrote %s" dest)
    (kill-buffer))
  (funcall post-install-fun package))

(defun el-get-http-install (package url post-install-fun &optional dest)
  "Dowload a single-file PACKAGE over HTTP and store it in DEST.

Should dest be omitted (nil), the url content will get written
into the package :localname option or its `file-name-nondirectory' part."
  (let* ((pdir   (el-get-package-directory package))
	 (fname  (or (plist-get (el-get-package-def package) :localname)
		     (el-get-filename-from-url url)))
	 (dest   (or dest
		     (concat (file-name-as-directory pdir) fname))))
    (unless (file-directory-p pdir)
      (make-directory pdir))

    (if (not el-get-default-process-sync)
        (url-retrieve url 'el-get-http-retrieve-callback
                      `(,package ,post-install-fun ,dest ,el-get-sources))

      (with-current-buffer (url-retrieve-synchronously url)
        (el-get-http-retrieve-callback
	 nil package post-install-fun dest el-get-sources)))))

(el-get-register-method
 :http #'el-get-http-install #'el-get-http-install #'el-get-rmdir
 #'el-get-http-install-hook)

(el-get-register-method
 :ftp #'el-get-http-install #'el-get-http-install #'el-get-rmdir
 #'el-get-http-install-hook)

(provide 'el-get-http)
