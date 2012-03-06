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

(require 'cl)
(require 'el-get-core)

(defvar el-get-status-file-cache-timestamp nil
  "Variable used to detect when the status file has changed on disk.

It holds the modification time of the status file as of the last
time *this* emacs process touched it. If the actual modification
time of the file does not match this, the cache is invalidated
and pacakge statuses are re-read from the file.")

(defvar el-get-status-file-cache nil
  "Cache variable used to avoid re-reading status file from disk.

This variable may safely be set to nil at any time. Doing so
would force the package statuses to be re-read from disk.")

(defun el-get-check-status-cache ()
  "Check if status file modtime has changed since last access.

If so, invalidate the cache. Returns the cache if still valid,
else nil."
  (let ((current-modtime (nth 5 (file-attributes el-get-status-file)))
        (cached-modtime el-get-status-file-cache-timestamp))
    (when (or (null cached-modtime)
              (not (equal current-modtime cached-modtime)))
      ;; Invalidate the cache, update the timestamp
      (setq el-get-status-file-cache-timestamp current-modtime
            el-get-status-file-cache nil)))
  ;; Return the cache, which may now be nil
  el-get-status-file-cache)

(defun el-get-save-package-status (package status)
  "Save given package status"
  (el-get-check-status-cache)
  (let* ((package (el-get-as-symbol package))
         (recipe (el-get-package-def package))
         (package-status-alist
          (assq-delete-all package (el-get-read-status-file)))
         (new-package-status-alist
          (sort (append package-status-alist
                        (list            ; alist of (PACKAGE . PROPERTIES-LIST)
                         (cons package (list 'status status 'recipe recipe))))
                (lambda (p1 p2)
                  (string< (el-get-as-string (car p1))
                           (el-get-as-string (car p2)))))))
    (with-temp-file el-get-status-file
      (pp new-package-status-alist (current-buffer)))
    ;; Cache and return the new alist, also updating the timestamp
    (setq el-get-status-file-cache-timestamp
          (nth 5 (file-attributes el-get-status-file))
          el-get-status-file-cache
          new-package-status-alist)))

(defun el-get-read-status-file ()
  "read `el-get-status-file' and return an alist of plist like:
   (PACKAGE . (status \"status\" recipe (:name ...)))"
  (el-get-check-status-cache)
  (or el-get-status-file-cache
      (setq
       el-get-status-file-cache
       (let ((ps
              (when (file-exists-p el-get-status-file)
                (car (with-temp-buffer
                       (insert-file-contents-literally el-get-status-file)
                       (read-from-string (buffer-string)))))))
         (if (consp (car ps))         ; check for an alist, new format
             ps)
         ;; convert to the new format, fetching recipes as we go
         (loop for (p s) on ps by 'cddr
               for x = (el-get-as-symbol (el-get-package-name p))
               when x
               collect (cons x (list 'status s
                                     'recipe (el-get-package-def x))))))))

(defun el-get-package-status-alist (&optional package-status-alist)
  "return an alist of (PACKAGE . STATUS)"
  (loop for (p . prop) in (or package-status-alist
                              (el-get-read-status-file))
        collect (cons p (plist-get prop 'status))))

(defun el-get-read-package-status (package &optional package-status-alist)
  "return current status for PACKAGE"
  (let ((p-alist (or package-status-alist (el-get-read-status-file))))
    (plist-get (cdr (assq (el-get-as-symbol package) p-alist)) 'status)))

(define-obsolete-function-alias 'el-get-package-status 'el-get-read-package-status)

(defun el-get-read-package-status-recipe (package &optional package-status-alist)
  "return current status for PACKAGE"
  (let ((p-alist (or package-status-alist (el-get-read-status-file))))
    (plist-get (cdr (assq (el-get-as-symbol package) p-alist)) 'recipe)))

(defun el-get-filter-package-alist-with-status (package-status-alist &rest statuses)
  "Return package names that are currently in given status"
  (loop for (p . prop) in package-status-alist
        for s = (plist-get prop 'status)
	when (member s statuses)
        collect (el-get-as-string p)))

(defun el-get-list-package-names-with-status (&rest statuses)
  "Return package names that are currently in given status"
  (apply #'el-get-filter-package-alist-with-status
         (el-get-read-status-file)
         statuses))

(defun el-get-read-package-with-status (action &rest statuses)
  "Read a package name in given status"
  (completing-read (format "%s package: " action)
                   (apply 'el-get-list-package-names-with-status statuses)))

(defun el-get-count-package-with-status (&rest statuses)
  "Return how many packages are currently in given status"
  (length (apply #'el-get-list-package-names-with-status statuses)))

(defun el-get-count-packages-with-status (packages &rest statuses)
  "Return how many packages are currently in given status in PACKAGES"
  (length (intersection
           (mapcar #'el-get-as-symbol (apply #'el-get-list-package-names-with-status statuses))
           (mapcar #'el-get-as-symbol packages))))

(defun el-get-extra-packages (&rest packages)
  "Return installed or required packages that are not in given package list"
  (let ((packages
	 ;; &rest could contain both symbols and lists
	 (loop for p in packages
	       when (listp p) append (mapcar 'el-get-as-symbol p)
	       else collect (el-get-as-symbol p))))
    (when packages
	(loop for (p . prop) in (el-get-read-status-file)
              for s = (plist-get prop 'status)
	      for x = (el-get-as-symbol (el-get-package-name p))
	      unless (member x packages)
	      unless (equal s "removed")
	      collect (list x s)))))

(provide 'el-get-status)
