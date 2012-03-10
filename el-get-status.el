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
(require 'pp)
(require 'el-get-core)

(defun el-get-package-name (package-symbol)
  "Returns a package name as a string."
  (cond ((keywordp package-symbol)
         (substring (symbol-name package-symbol) 1))
        ((symbolp package-symbol)
         (symbol-name package-symbol))
        ((stringp package-symbol)
         package-symbol)
        (t (error "Unknown package: %s"))))

(defun el-get-package-symbol (package)
  "Returns a package name as a non-keyword symbol"
  (cond ((keywordp package)
         (intern (substring (symbol-name package) 1)))
        ((symbolp package)
         package)
        ((stringp package) (intern package))
        (t (error "Unknown package: %s"))))

(defun el-get-package-keyword (package-name)
  "Returns a package name as a keyword :package."
  (if (keywordp package-name)
      package-name
    (intern (format ":%s" package-name))))

(defun el-get-save-package-status (package status &optional recipe)
  "Save given package status"
  (let* ((package (el-get-as-symbol package))
         (recipe
          (or recipe
              (when (string= status "installed")
                (el-get-package-def package)))
         (package-status-alist
          (assq-delete-all package (el-get-read-status-file)))
         (new-package-status-alist
          (sort (append package-status-alist
                        (list            ; alist of (PACKAGE . PROPERTIES-LIST)
                         (cons package (list 'status status 'recipe recipe))))
                (lambda (p1 p2)
                  (string< (el-get-as-string (car p1))
                           (el-get-as-string (car p2))))))
         print-level print-length)
    (with-temp-file el-get-status-file
      (insert (el-get-print-to-string new-package-status-alist 'pretty)))
    ;; Return the new alist
    new-package-status-alist))

(defun el-get-read-status-file ()
  "read `el-get-status-file' and return an alist of plist like:
   (PACKAGE . (status \"status\" recipe (:name ...)))"
  (let ((ps
         (when (file-exists-p el-get-status-file)
           (car (with-temp-buffer
                  (insert-file-contents-literally el-get-status-file)
                  (read-from-string (buffer-string)))))))
    (if (consp (car ps))         ; check for an alist, new format
        ps
      ;; convert to the new format, fetching recipes as we go
      (loop for (p s) on ps by 'cddr
            for psym = (el-get-package-symbol p)
            when psym
            collect (cons psym
                          (list 'status s
                                'recipe (when (string= s "installed")
                                          (el-get-package-def psym))))))))

(defun el-get-package-status-alist (&optional package-status-alist)
  "return an alist of (PACKAGE . STATUS)"
  (loop for (p . prop) in (or package-status-alist
                              (el-get-read-status-file))
        collect (cons p (plist-get prop 'status))))

(defun el-get-package-status-recipes (&optional package-status-alist)
  "return the list of recipes stored in the status file"
  (loop for (p . prop) in (or package-status-alist
                              (el-get-read-status-file))
        when (string= (plist-get prop 'status) "installed")
        collect (plist-get prop 'recipe)))

(defun el-get-read-package-status (package &optional package-status-alist)
  "return current status for PACKAGE"
  (let ((p-alist (or package-status-alist (el-get-read-status-file))))
    (plist-get (cdr (assq (el-get-as-symbol package) p-alist)) 'status)))

(define-obsolete-function-alias 'el-get-package-status 'el-get-read-package-status)

(defun el-get-read-package-status-recipe (package &optional package-status-alist)
  "return current status recipe for PACKAGE"
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
            for x = (el-get-package-symbol p)
            unless (member x packages)
            unless (equal s "removed")
            collect (list x s)))))

(defmacro el-get-with-status-sources (&rest body)
  "Evaluate BODY with `el-get-sources' bound to recipes from status file."
  `(let ((el-get-sources (el-get-package-status-recipes)))
     (progn ,@body)))
(put 'el-get-with-status-sources 'lisp-indent-function
     (get 'progn 'lisp-indent-function))

(defvar el-get-status-recipe-updatable-properties
  '(:load-path
    :info
    :load
    :features
    :library
    :before
    :after
    :lazy)
  "Whitelist of properties that may be updated in cached recipes.

If you any have these properties set on a package in your
`el-get-sources', then the values from `el-get-sources' can be
used to replace the cached values without reinstalling or
updating the package..")

(defun el-get-merge-updatable-properties (package-or-source
                                          &optional ignore-non-updatable
                                          package-status-alist)
  "Merge updatable properties from package source into status file.

This function takes either a package name or a full package
source. The named package must already be installed. If given a
package name, the source is retrieved from `el-get-sources'. The
given source is compared to the cached source for the same
package. If it differs only in updatable properties (see
`el-get-status-recipe-updatable-properties'), then the updated
values from the given source will be saved to the recipe in the
status file, overwriting the old values stored there.

If any non-updatable properties differ, then an error is raised,
unless the optional second argument is non-nil, in which case
only a message is issued and the non-updatable properties are
simply ignored.

Normally, the updated recipe is written back to the status
file. However, a package status alist may be provided as an
optional third argument. If this is non-nil the recipe will be
updated in place by modifying the alist (it is not returned), and
the status file will not be modified. When run interactively, the
updated recipe is always saved to the status file."
  (interactive
   (list (el-get-read-package-with-status "Update cached recipe" "installed")))
  (let* ((source
          (if (listp package-or-source)
              (or package-or-source
                  (error "package-or-source cannot be nil"))
            ;; Not using `el-get-package-def' here, because we only
            ;; want what is listed in `el-get-sources', not what is in
            ;; the recipe file.
            (loop for src in el-get-sources
                  when (string= package-or-source (el-get-source-name src))
                  if (symbolp src) return (list :name src)
                  else return src)))
         (pkg (el-get-as-symbol (el-get-source-name source)))
         (cached-recipe (or (el-get-read-package-status-recipe
                             pkg
                             package-status-alist)
                            (error "Could not retrieve cached recipe for package %s"
                                   package)))
         ;; Subset of properties that cannot be updated without a
         ;; package update or reinstall. If this is non-nil, then we
         ;; will abort with an error message.
         (noupdate-plist
          (loop for (k v) on source by 'cddr
                unless (eq k :name)
                unless (memq k el-get-status-recipe-updatable-properties)
                ;; We only care about non-updatable properties if they
                ;; don't match the cached value
                unless (equal v (plist-get cached-recipe k))
                append (list k v)))
         ;; Subset of properties that are updatable. If this is nil,
         ;; then there's nothing to update.
         (update-plist
          (loop for (k v) on source by 'cddr
                when (memq k el-get-status-recipe-updatable-properties)
                append (list k v))))
    (unless (el-get-package-is-installed pkg)
      (error "Package %s is not installed. Cannot update recipe." pkg))
    (when noupdate-plist
      (funcall
       (if ignore-non-updatable
           'el-get-verbose-message
         'error)
       "Cannot update the following properties on package %s:\n%s\n(Maybe you should use `el-get-update' or `el-get-reinstall' instead?)"
       pkg (pp-to-string noupdate-plist)))
    (if update-plist
        (progn
          (el-get-verbose-message
           "Updating the following properties on package %s:\n%s"
           pkg (pp-to-string update-plist))
          (let ((updated-recipe (el-get-read-package-status-recipe pkg)))
            (loop for (k v) on update-plist by 'cddr
                  do (plist-put updated-recipe k v))
            (if package-status-alist
                ;; If alist was provided, modify it in place
                (plist-put (cdr (assq package package-status-alist))
                           'recipe updated-recipe)
              ;; Else save to status file
              (el-get-save-package-status pkg "installed" updated-recipe))))
      (el-get-verbose-message "No properties to update on package %s" pkg))))

(provide 'el-get-status)
