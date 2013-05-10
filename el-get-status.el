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
        (t (error "Unknown package: %s" package-symbol))))

(defun el-get-package-symbol (package)
  "Returns a package name as a non-keyword symbol"
  (cond ((keywordp package)
         (intern (substring (symbol-name package) 1)))
        ((symbolp package)
         package)
        ((stringp package) (intern package))
        (t (error "Unknown package: %s" package))))

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
                (el-get-package-def package))))
         (package-status-alist
          (assq-delete-all package (el-get-read-status-file)))
         (new-package-status-alist
          (sort
           ;; Do not save package information if status is removed.
           (if (string= status "removed")
               package-status-alist
             (append package-status-alist
                     (list  ; alist of (PACKAGE . PROPERTIES-LIST)
                      (cons package (list 'status status 'recipe recipe)))))
                (lambda (p1 p2)
                  (string< (el-get-as-string (car p1))
                           (el-get-as-string (car p2)))))))
    (assert (listp recipe) nil
            "Recipe must be a list")
    (with-temp-file el-get-status-file
      (insert (el-get-print-to-string new-package-status-alist 'pretty)))
    ;; Update cache
    (setq el-get-status-cache new-package-status-alist)
    ;; Return the new alist
    new-package-status-alist))

(defun el-get-convert-from-old-status-format (old-status-list)
  "Convert OLD-STATUS-LIST, a property list, to the new format"
  ;; first backup the old status just in case
  (with-temp-file (format "%s.old" el-get-status-file)
    (insert (el-get-print-to-string ps)))
  ;; now convert to the new format, fetching recipes as we go
  (loop for (p s) on ps by 'cddr
        for psym = (el-get-package-symbol p)
        when psym
        collect
        (cons psym
              (list 'status s
                    'recipe (when (string= s "installed")
                              (condition-case nil
                                  (el-get-package-def psym)
                                ;; If the recipe is not available any more,
                                ;; just provide a placeholder no-op recipe.
                                (error `(:name ,psym :type builtin))))))))

(defvar el-get-status-cache nil
  "Cache used by `el-get-read-status-file'.")

(defun el-get-clear-status-cache ()
  "Clear in-memory cache for status file."
  (setq el-get-status-cache nil))

(defun el-get-read-status-file ()
  "read `el-get-status-file' and return an alist of plist like:
   (PACKAGE . (status \"status\" recipe (:name ...)))"
  (or el-get-status-cache
      (setq el-get-status-cache (el-get-read-status-file-force))))

(defun el-get-read-status-file-force ()
  "Forcefully load status file."
  (let* ((ps
          (when (file-exists-p el-get-status-file)
            (car (with-temp-buffer
                   (insert-file-contents-literally el-get-status-file)
                   (read-from-string (buffer-string))))))
         (p-s
          (if (consp (car ps))         ; check for an alist, new format
              ps
            (el-get-convert-from-old-status-format ps))))
    ;; double check some status "conditions"
    ;;
    ;; a package with status "installed" and a missing directory is
    ;; automatically reset to "required" so that a proper install happens.
    (loop for (p . prop) in p-s
          if (and (string= (plist-get prop 'status) "installed")
                  (not (file-directory-p (el-get-package-directory p))))
          collect (cons p (plist-put prop 'status "required"))
          else
          collect (cons p prop))))

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

(defmacro el-get-with-status-sources (package-status-alist &rest body)
  "Evaluate BODY with `el-get-sources' bound to recipes from PACKAGE-STATUS-ALIST.

If PACKAGE-STATUS-ALIST is nil, read recipes from status file."
  `(let ((el-get-sources (el-get-package-status-recipes package-status-alist)))
     (progn ,@body)))
(put 'el-get-with-status-sources 'lisp-indent-function
     (get 'prog1 'lisp-indent-function))

(defvar el-get-status-recipe-update-whitelist
  '(:load-path
    :info
    :load
    :features
    :library
    :prepare
    :before
    :after
    :post-init
    :lazy
    :website
    :description)
  "Properties that can be updated without `el-get-update'/`el-get-reinstall'.

If any of these properties change on the recipe for an installed
package, the changes may be merged into the cached version of
that recipe in the el-get status file.")

(defun el-get-classify-new-properties (source newprops)
  "Classify NEWPROPS into two groups: need update/disallowed.

Return a list (UPDATE DISALLOWED).  Both UPDATE and NEWPROPS are
subset of NEWPROPS whose element is different from SOURCE.
Elements in UPDATE are whitelisted whereas elements in DISALLOWED
are not."
  (loop with update
        with disallowed
        with whitelist = el-get-status-recipe-update-whitelist
        for (k v) on newprops by 'cddr
        ;; Not a change, so ignore it
        if (equal v (plist-get source k)) do (ignore)
        ;; whitelisted
        else if (memq k whitelist) do (setq update (plist-put update k v))
        ;; Trying to change non-whitelisted property
        else do (setq disallowed (plist-put disallowed k v))
        finally return (list update disallowed)))

(defun el-get-diagnosis-properties (old-source new-source)
  "Diagnosis difference between OLD-SOURCE and NEW-SOURCE.

Return a list (UPDATE-P ADDED-DISALLOWED REMOVED-DISALLOWED).
UPDATE-P is non-nil when OLD-SOURCE and NEW-SOURCE are different.
ADDED-DISALLOWED and REMOVED-DISALLOWED are added and removed
properties, respectively."
  (let ((added   (el-get-classify-new-properties old-source new-source))
        (removed (el-get-classify-new-properties new-source old-source)))
    (list (or (car added) (car removed))
          (cadr added)
          (cadr removed))))

(defun el-get-package-or-source (package-or-source)
  "Given either a package name or a full source entry, return a
   full source entry."
  (if (listp package-or-source)
      (or package-or-source
          (error "package-or-source cannot be nil"))
    (el-get-package-def package-or-source)))

(defun el-get-read-cached-recipe (package source &optional package-status-alist)
  "Read the cached recipe for given PACKAGE: the one we have in the status file.

   If given PACKAGE isn't registered in the status file, and if
   it's a builtin package, then install it."
  (or (el-get-read-package-status-recipe package package-status-alist)
      (if (eq 'builtin (el-get-package-method source))
          (let ((el-get-default-process-sync t))
            (el-get-install package))
        ;; it's not builtin, it's not installed.
        (error "Package %s is nowhere to be found in el-get status file."
               package))))

(defun* el-get-merge-properties-into-status (package-or-source
                                             &optional package-status-alist
                                             &key noerror)
  "Merge updatable properties for package into package status alist (or status file).

The first argument is either a package source or a package name,
in which case the source will be read using
`el-get-package-def'. The named package must already be
installed.

If the new source differs only in whitelisted properties (see
`el-get-status-recipe-updatable-properties'), then the updated
values for those properties will be incorporated into the
package's recipe from PACKAGE-STATUS-ALIST, which is modified in
place. If PACKAGE-STATUS-ALIST is not given, it will be read from
the status file and the modifications will be written to the
status file. When run interactively, the updated recipe is always
saved to the status file.

If any non-whitelisted properties differ from the cached values,
then an error is raise. With optional keyword argument `:noerror
t', this error is suppressed (but nothing is updated)."
  (interactive
   (list (el-get-read-package-with-status "Update cached recipe" "installed")
         nil
         :noerror current-prefix-arg))
  (let* ((save-to-file (null package-status-alist))
         (source       (el-get-package-or-source package-or-source))
         (package      (el-get-as-symbol (el-get-source-name source)))
         (cached-recipe
          (el-get-read-cached-recipe package source package-status-alist)))
    (unless (el-get-package-is-installed package)
      (error "Package %s is not installed. Cannot update recipe." package))
    (destructuring-bind (update-p added-disallowed removed-disallowed)
        (el-get-diagnosis-properties cached-recipe source)
      (when (or added-disallowed removed-disallowed)
        ;; Emit a verbose message if `noerror' is t (but still quit
        ;; the function).
        (funcall (if noerror 'el-get-verbose-message 'error)
                 "Tried to add non-whitelisted properties:

%s

and remove non-whitelisted properties:

%s

into/from source:

%s
Maybe you should use `el-get-update' or `el-get-reinstall' on %s instead?"
                 (if   added-disallowed (pp-to-string   added-disallowed) "()")
                 (if removed-disallowed (pp-to-string removed-disallowed) "()")
                 (pp-to-string cached-recipe)
                 (el-get-source-name cached-recipe))
        (return-from el-get-merge-properties-into-status))
      (when update-p
        (if save-to-file
            (el-get-save-package-status package "installed" source)
          (plist-put (cdr (assq package package-status-alist))
                     'recipe source))))))

(provide 'el-get-status)
