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
                (el-get-package-def package))))
         (package-status-alist
          (assq-delete-all package (el-get-read-status-file)))
         (new-package-status-alist
          (sort (append package-status-alist
                        (list            ; alist of (PACKAGE . PROPERTIES-LIST)
                         (cons package (list 'status status 'recipe recipe))))
                (lambda (p1 p2)
                  (string< (el-get-as-string (car p1))
                           (el-get-as-string (car p2)))))))
    (assert (listp recipe) nil
            "Recipe must be a list")
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
    :before
    :after
    :lazy)
  "Whitelist of properties that may be updated in cached recipes.

If any of these properties change on the recipe for an installed
package, the changes may be merged into the cached version of
that recipe in the el-get status file.")

(defun el-get-merge-whitelisted-properties (source newprops)
  "Merge NEWPROPS into SOURCE if they are all whitelisted.

SOURCE should be an el-get package recipe, and NEWPROPS should be
a plist to be merged with it (possibly a full recipe, but not
necessarily). Every property in NEWPROPS that must either be
whitelisted or must be identical to the corresponding property in
SOURCE.

Returns the merged source without modifying the original.

To see which properties are whitelisted, see
`el-get-status-recipe-update-whitelist'."
  (let* ((whitelist el-get-status-recipe-update-whitelist)
        (updated-source (copy-list source))
        (disallowed-props
         (loop for (k v) on newprops by 'cddr
               ;; whitelisted
               if (memq k whitelist) do (plist-put updated-source k v)
               ;; Not a change, so ignore it
               else if (equal v (plist-get source k)) do (ignore)
               ;; Trying to change non-whitelisted property
               else append (list k v))))
    ;; Raise an error if we tried to set any disallowed
    ;; properties. (We wait until now so we can report the full list.)
    (when disallowed-props
      (error (format "Tried to merge non-whitelisted properties:

%s
into source:

%s
Maybe you should use `el-get-update' or `el-get-reinstall' on %s instead?"
                     (pp-to-string disallowed-props)
                     (pp-to-string source)
                     (el-get-source-name source))))
    updated-source))

(defun* el-get-merge-properties-into-status (package-or-source
                                             &optional package-status-alist
                                             &key noerror skip-non-updatable)
  "Merge updatable properties for package into pacakge status alist (or status file).

The first argument is either a package source or a pacakge name,
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
t', this error is suppressed (but nothing is updated). With
optional keyword argument `:skip-non-updatable t', the
whitelisted recipe changes will be merged despite the presence of
non-whitelisted changes, and no error will be raised.
"
  (interactive
   (list (el-get-read-package-with-status "Update cached recipe" "installed")
         nil
         :noerror current-prefix-arg))
  (let* ((save-to-file (null package-status-alist))
         (source
          (if (listp package-or-source)
              (or package-or-source
                  (error "package-or-source cannot be nil"))
            (el-get-package-def package-or-source)))
         (package (el-get-as-symbol (el-get-source-name source)))
         (cached-recipe (or (el-get-read-package-status-recipe
                             package
                             package-status-alist)
                            (error "Could not retrieve cached recipe for package %s"
                                   package))))
    (unless (el-get-package-is-installed package)
      (error "Package %s is not installed. Cannot update recipe." package))
    (when skip-non-updatable
      ;; Filter out non-whitelisted properties now to avoid the error
      ;; later.
      (setq source
            (loop for (k v) on source by 'cddr
                  when (memq k el-get-status-recipe-update-whitelist)
                  append (list k v))))
    (let ((merged-recipe
           (condition-case err
               (el-get-merge-whitelisted-properties cached-recipe source)
             ;; Convert the error to a verbose message if `noerror' is
             ;; t (but still quit the function).
             (error
              (progn
                (apply (if noerror 'el-get-verbose-message 'error)
                       (cdr err))
                ;; Return from the function even if no error was thrown
                (return-from el-get-merge-properties-into-status)))
             )))
      (if save-to-file
          (el-get-save-package-status package "installed" merged-recipe)
        (plist-put (cdr (assq package package-status-alist))
                   'recipe merged-recipe)))))

(provide 'el-get-status)
