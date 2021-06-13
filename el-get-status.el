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

(declare-function el-get-install "el-get" (package))
(declare-function el-get-package-is-installed "el-get" (package))
(declare-function el-get-print-package "el-get-list-packages" (package-name status &optional desc))

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

(defvar el-get-status-cache nil
  "Cache used by `el-get-read-status-file'.")

(defvar el-get-package-menu-buffer) ; from el-get-list-packages.el
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
    ;; Update package menu, if it exists
    (save-excursion
      (when (and (bound-and-true-p el-get-package-menu-buffer)
                 (buffer-live-p el-get-package-menu-buffer)
                 (set-buffer el-get-package-menu-buffer)
                 (eq major-mode 'el-get-package-menu-mode))
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (name (el-get-package-name package)))
          (when (re-search-forward
                 (format "^..%s[[:blank:]]+[^[:blank:]]+[[:blank:]]+"
                         (regexp-quote name)) nil t)
            (delete-region (match-beginning 0) (match-end 0))
            (el-get-print-package name status)))))
    ;; Return the new alist
    new-package-status-alist))

(defun el-get-convert-from-old-status-format (old-status-list)
  "Convert OLD-STATUS-LIST, a property list, to the new format"
  ;; first backup the old status just in case
  (with-temp-file (format "%s.old" el-get-status-file)
    (insert (el-get-print-to-string old-status-list)))
  ;; now convert to the new format, fetching recipes as we go
  (loop for (p s) on old-status-list by 'cddr
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
          (if (file-exists-p el-get-status-file)
              (car (with-temp-buffer
                     (insert-file-contents-literally el-get-status-file)
                     (read-from-string (buffer-string))))
            ;; If it doesn't exist, make sure the directory is there
            ;; so we can create it.
            (make-directory el-get-dir t)))
         (p-s
          (cond
           ((null ps) ;; nothing installed, we should install el-get
            (list (list 'el-get 'status "required")))
           ;; ps is an alist, no conversion needed
           ((consp (car ps)) ps)
           ;; looks like we might have an old format status list
           (t (el-get-convert-from-old-status-format ps)))))
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

(defun el-get-package-status-alist ()
  "return an alist of (PACKAGE . STATUS)"
  (loop for (p . prop) in (el-get-read-status-file)
        collect (cons p (plist-get prop 'status))))

(defun el-get-package-status-recipes ()
  "return the list of recipes stored in the status file"
  (loop for (p . prop) in (el-get-read-status-file)
        when (string= (plist-get prop 'status) "installed")
        collect (plist-get prop 'recipe)))

(defun el-get-read-package-status (package)
  "return current status for PACKAGE"
  (plist-get (cdr (assq (el-get-as-symbol package) (el-get-read-status-file)))
             'status))

(define-obsolete-function-alias 'el-get-package-status 'el-get-read-package-status "4.1")

(defun el-get-read-package-status-recipe (package)
  "return current status recipe for PACKAGE"
  (plist-get (cdr (assq (el-get-as-symbol package) (el-get-read-status-file)))
             'recipe))

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

(defmacro el-get-with-status-sources (_ &rest body)
  "Evaluate BODY with `el-get-sources' according to the status file."
  (declare (debug t) (indent 1))
  `(let ((el-get-sources (el-get-package-status-recipes)))
     (progn ,@body)))


(defconst el-get-status-init-whitelist
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
  "Properties that can be updated with only `el-get-init'.

If any of these properties change on the recipe for an installed
package, the changes may be merged into the cached version of
that recipe in the el-get status file.")

(defconst el-get-status-update-whitelist
  `(:depends
    :build
    ;; :build/* ; special cased below
    :compile
    :checksum
    :checkout
    :options
    ,@el-get-status-init-whitelist)
  "Properties than can be updated by `el-get-update'.")

(defun el-get-classify-new-properties (source newprops)
  "Determine the operations required to update SOURCE with NEWPROPS.

Partition the properties of NEWPROPS whose value is different
from SOURCE into 3 sublists, (INIT UPDATE REINSTALL), according
to the operation required."
  (loop with init and update and reinstall
        with type = (let ((old-type (el-get-package-method source))
                          (new-type (el-get-package-method newprops)))
                      (if (eq old-type new-type) old-type nil))
        for (k v) on newprops by 'cddr
        if (equal v (plist-get source k)) do (ignore) ; Ignore non-changes.
        else if
        (or (memq k el-get-status-init-whitelist)
            (if (eq k :builtin) ; `:builtin' safe if not crossing versions.
                (eq (version<= emacs-version (el-get-as-string v))
                    (version<= emacs-version (el-get-as-string
                                              (plist-get source k))))))
        do (setq init (plist-put init k v))
        else if (or (memq k el-get-status-update-whitelist)
                    ;; All `:build/*' props are update safe, like `:build'.
                    (string-prefix-p ":build/" (symbol-name k))
                    (if (eq k :url) ; `:http*' methods can handle `:url' changes.
                        (memq type '(http http-tar http-zip
                                          github-tar github-zip
                                          builtin))))
        do (setq update (plist-put update k v))
        else do (setq reinstall (plist-put reinstall k v))
        finally return (list init update reinstall)))

(defun el-get-compute-new-status (operation old new)
  "Return an update of OLD with NEW.

Return a list (RESULT REQUIRED TO-ADD TO-REM), where RESULT is
the updated recipe.  TO-ADD and TO-REM are the list properties
that prevent a full update with the given OPERATION, REQUIRED is
a list of operations that would allow a full update."
  (let* ((ops       '(init update reinstall))
         (op-rank   (1- (length (memq operation ops))))
         (ops-given (butlast ops op-rank))
         (rem-props (el-get-classify-new-properties new old))
         (add-props (el-get-classify-new-properties old new))
         (rem-allow (apply #'append (butlast rem-props op-rank)))
         (add-allow (apply #'append (butlast add-props op-rank)))
         (no-rem    (last rem-props op-rank))
         (no-add    (last add-props op-rank)))
    (list (nconc (loop for (key val) on old by #'cddr
                       unless (plist-member rem-allow key)
                       nconc (list key val))
                 add-allow)
          (loop for i from (1- (length ops)) downto (length ops-given)
                when (or (nth i rem-props) (nth i add-props))
                return (nthcdr i ops))
          (apply #'append no-add) (apply #'append no-rem))))

(defun el-get-package-or-source (package-or-source)
  "Given either a package name or a full source entry, return a
   full source entry."
  (if (listp package-or-source)
      (or package-or-source
          (error "package-or-source cannot be nil"))
    (el-get-package-def package-or-source)))

(defun el-get-read-cached-recipe (package source)
  "Read the cached recipe for given PACKAGE: the one we have in the status file.

   If given PACKAGE isn't registered in the status file, and if
   it's a builtin package, then install it."
  (or (el-get-read-package-status-recipe package)
      (if (eq 'builtin (el-get-package-method source))
          (let ((el-get-default-process-sync t))
            (el-get-install package))
        ;; it's not builtin, it's not installed.
        (error "Package %s is nowhere to be found in el-get status file."
               package))))

(el-get-define-pkg-op-button-type 'el-get-merge-properties-into-status
                                  "force cached recipe update of")

(defun el-get-merge-properties-into-status (package operation &rest keys)
  "Merge updatable properties for package into status file.

PACKAGE is either a package source or name, in which case the
source will be read using `el-get-package-def'.  The named
package must already be installed.

Warn about any non-whitelisted for OPERATION properties differing
from the cached values.

Interactively, OPERATION is `update' with prefix arg, `reinstall'
with double prefix arg, or `init' otherwise."
  (interactive
   (list (el-get-read-package-with-status "Update cached recipe" "installed")
         (cond ((equal '(16) current-prefix-arg) 'reinstall)
               (current-prefix-arg 'update)
               (t 'init))))
  (let* ((source       (el-get-package-or-source package))
         (package      (plist-get source :name))
         (cached       (el-get-read-cached-recipe package source)))
    (unless (el-get-package-is-installed package)
      (error "Package %s is not installed. Cannot update recipe." package))
    (destructuring-bind (new-src required-ops no-add no-rem)
        (el-get-compute-new-status operation cached source)
      (el-get-save-package-status package "installed" new-src)
      (when required-ops
        (lwarn '(el-get recipe-cache) :warning
               (concat "Must %s `%s' to modify its cached recipe\n"
                       "  adding:   %s"
                       "  removing: %s"
                       (el-get-fmt-button
                        "  Or %s if you know these changes are safe.\n"
                        "force update the cached recipe"
                        :type 'el-get-merge-properties-into-status
                        'el-get-package package 'el-get-pkg-extra-args '(reinstall)))
               (mapconcat (lambda (op)
                            (el-get-fmt-button
                             "%s" op :type (intern (concat "el-get-" op))
                             'el-get-package package))
                          (mapcar #'symbol-name required-ops) " or ")
               package
               (if no-add (pp-to-string no-add) "()\n")
               (if no-rem (pp-to-string no-rem) "()\n"))))))

;; Using `declare' in `defun' only supported from Emacs 24.3.
(set-advertised-calling-convention
 'el-get-merge-properties-into-status '(package operation) "May 2016")

(provide 'el-get-status)
