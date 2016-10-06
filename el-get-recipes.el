;;; el-get-recipes.el --- Manage the external elisp bits and pieces you depend upon
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

;;; Commentary:
;;
;; el-get-recipes provides the API to manage the el-get recipes.
;;

;; el-get-core provides basic el-get API, intended for developers of el-get
;; and its methods.  See the methods directory for implementation of them.
;;

;;; Code:

(require 'el-get-core)
(require 'el-get-custom)
(require 'el-get-byte-compile)

(defcustom el-get-recipe-path-emacswiki
  (expand-file-name "el-get/recipes/emacswiki/" el-get-dir)
  "Define where to keep a local copy of emacswiki recipes."
  :group 'el-get
  :type 'directory)

(defcustom el-get-recipe-path-elpa
  (expand-file-name "el-get/recipes/elpa/" el-get-dir)
  "Define where to keep a local copy of elpa recipes."
  :group 'el-get
  :type 'directory)

(defvar el-get-recipe-path
  (list (concat (file-name-directory el-get-script) "recipes")
        el-get-recipe-path-elpa
        el-get-recipe-path-emacswiki)
  "List of directories in which to look for el-get recipes.

Directories that contain automatically-generated recipes, such as
`el-get-recipe-path-emacswiki' and `el-get-recipe-path-elpa',
should be placed last in this list.

This variable is not customizable, as it needs to be set before
el-get is loaded, while customizations should be loaded after
el-get, so that they can affect pacakages loaded by el-get.
It is recommended to add new directories using code like:

  (add-to-list 'el-get-recipe-path \"~/.emacs.d/el-get-user/recipes/\")")

(defcustom el-get-user-package-directory nil
  "Define where to look for init-pkgname.el configurations.  Disabled if nil."
  :group 'el-get
  :type '(choice (const :tag "Off" nil) directory))

(defun el-get-load-package-user-init-file (package)
  "Load the user init file for PACKAGE, called init-package.el
and to be found in `el-get-user-package-directory'.  Do nothing
when this custom is nil.

Will automatically compile the init file as needed and load the
compiled version."
  (when el-get-user-package-directory
    (let* ((init-file-name (format "init-%s.el" package))
           (package-init-file
            (expand-file-name init-file-name el-get-user-package-directory))
           (file-name-no-extension (file-name-sans-extension package-init-file))
           (compiled-init-file (concat file-name-no-extension ".elc"))
           (default-directory (el-get-package-directory package)))
      (when (file-exists-p package-init-file)
        (when el-get-byte-compile
          (el-get-byte-compile-file package-init-file))
        (el-get-verbose-message "el-get: load %S" file-name-no-extension)
        (load file-name-no-extension 'noerror)))))

(defun el-get-recipe-dirs ()
  "Return the elements of `el-get-recipe-path' that actually exist.

Used to avoid errors when exploring the path for recipes"
  (reduce (lambda (dir result)
            (if (file-directory-p dir) (cons dir result) result))
          el-get-recipe-path :from-end t :initial-value nil))

;; recipe files are elisp data, you can't byte-compile or eval them on their
;; own, but having elisp indenting and colors make sense
(eval-and-compile
  (add-to-list 'auto-mode-alist '("\\.rcp\\'" . emacs-lisp-mode)))

;;
;; recipes
;;

(defun el-get-recipe-pprint (source)
  "Pretty print a recipe definition to current buffer."
  (if (eq major-mode 'emacs-lisp-mode)
      (let ((point (point)))
        (insert "(")
        ;; Standard Emacs pretty print functions don't put newlines after
        ;; prop val pairs of plists, so we have to do it ourselves.
        (loop for (prop val) on source by #'cddr
              do (insert (format "%S %S\n" prop val)))
        (delete-char -1)                ; delete last \n
        (insert ")\n")
        (goto-char point)
        (indent-pp-sexp 'pretty))
    (let ((temp-buffer (generate-new-buffer " *temp*")))
      (unwind-protect
          (progn
            (save-current-buffer
              (set-buffer temp-buffer)
              (emacs-lisp-mode)
              (el-get-recipe-pprint source))
            (insert-buffer-substring temp-buffer))
        (kill-buffer temp-buffer)))))

(defun el-get-recipe-filename (package)
  "Return the name of the file that contains the recipe for PACKAGE, if any."
  (let ((package-el  (concat (el-get-as-string package) ".el"))
        (package-rcp (concat (el-get-as-string package) ".rcp")))
    (loop for dir in el-get-recipe-path
          for recipe-el  = (expand-file-name package-el dir)
          for recipe-rcp = (expand-file-name package-rcp dir)
          if (file-exists-p recipe-el)  return recipe-el
          if (file-exists-p recipe-rcp) return recipe-rcp)))

(defun el-get-read-recipe (package)
  "Return the source definition for PACKAGE, from the recipes."
  (let ((filename (el-get-recipe-filename package)))
    (if filename
        (el-get-read-from-file filename)
      (error "El-get can not find a recipe for package \"%s\"" package))))

(defun el-get-all-recipe-file-names ()
  "Return the list of all file based recipe names.

The result may have duplicates."
  (loop for dir in (el-get-recipe-dirs)
        nconc (mapcar #'file-name-base
                      (directory-files dir nil "^[^.].*\.\\(rcp\\|el\\)$"))))

(defun el-get-read-all-recipe-files ()
  "Return the list of all file based recipes, formated like `el-get-sources'.

Only consider any given recipe only once even if present in
multiple dirs from `el-get-recipe-path'. The first recipe found
is the one considered."
  (let (packages)
    (loop
     for dir in (el-get-recipe-dirs)
     nconc (loop
            for recipe in (directory-files dir nil "^[^.].*\.\\(rcp\\|el\\)$")
            for filename = (concat (file-name-as-directory dir) recipe)
            for pname = (file-name-sans-extension
                         (file-name-nondirectory recipe))
            for package = (el-get-as-symbol pname)
            unless (member package packages)
            do (push package packages)
            and collect (ignore-errors (el-get-read-from-file filename))))))

(defun el-get-read-all-recipes ()
  "Return the list of all the recipes, formatted like `el-get-sources'.

  We first look in `el-get-sources' then in each directory listed
in `el-get-recipe-path' in order."
  (let* ((s-recipes (mapcar (lambda (s) (cons (plist-get s :name) s))
                            el-get-sources))
         (file-recipes (el-get-read-all-recipe-files)))
    (setq file-recipes
          (mapcar (lambda (f-recipe)
                    (let* ((pkg (plist-get f-recipe :name))
                           (s-recipe (assq pkg s-recipes)))
                      (if (null s-recipe) f-recipe
                        (setq s-recipes (assq-delete-all pkg s-recipes))
                        (el-get-recipe-merge f-recipe (cdr s-recipe)))))
                  file-recipes))
    (append (mapcar #'cdr s-recipes) file-recipes)))

(defun el-get-recipe-merge (r1 r2)
  "Merge recipe R1 into R2.

If R2 has a `:type' it completely replaces R1, otherwise, R1
fields are the default value and R2 may override them."
  (if (plist-get r2 :type)
      r2
    (loop with merged
          for (prop val) on (append r2 r1) by 'cddr
          unless (plist-member merged prop)
          nconc (list prop val) into merged
          finally return merged)))

(defun el-get-package-def (package)
  "Return a single `el-get-sources' entry for PACKAGE."
  (let ((source (loop for src in el-get-sources
                      when (string= package (el-get-source-name src))
                      return src)))

    (cond ((or (null source) (symbolp source))
           ;; not in `el-get-sources', or only mentioned by name
           ;; (compatibility from pre 3.1 era)
           (el-get-read-recipe package))

          ((null (plist-get source :type))
           ;; we got a list with no :type, that's an override plist
           (el-get-recipe-merge (el-get-read-recipe package) source))
          ;; none of the previous, must be a full definition
          (t source))))

(defun el-get-package-method (package-or-source)
  "Return the :type property (called method) of PACKAGE-OR-SOURCE.

If the package is built in to the current major version of Emacs,
return 'builtin."
  (let* ((def (if (or (symbolp package-or-source) (stringp package-or-source))
                  (el-get-package-def package-or-source)
                package-or-source))
         (builtin (plist-get def :builtin)))

    (when (integerp builtin)
      (warn "Integer argument for :builtin is obsolete.  Use strings instead.")
      (setq builtin (number-to-string builtin)))
    (if (and builtin (version<= builtin emacs-version))
        'builtin
      (plist-get def :type))))

(defalias 'el-get-package-type #'el-get-package-method)

(defun el-get-package-types-alist (statuses &rest types)
  "Return an alist of package names that are of given TYPES.

Only consider packages whose status is `member' of STATUSES,
which defaults to installed, required and removed.  Example:

  (el-get-package-types-alist \"installed\" 'http 'cvs)"
  (loop for src in (apply 'el-get-list-package-names-with-status
                          (cond ((stringp statuses) (list statuses))
                                ((null statuses) '("installed" "required"
                                                   "removed"))
                                (t statuses)))
        for name = (el-get-as-symbol src)
        for type = (el-get-package-type name)
        when (or (null types) (memq 'all types) (memq type types))
        collect (cons name type)))

(defun el-get-package-required-emacs-version (package-or-source)
  (let* ((def (if (or (symbolp package-or-source) (stringp package-or-source))
                  (el-get-package-def package-or-source)
                package-or-source)))
    (el-get-plist-get-with-default
        def :minimum-emacs-version
      0)))

(defun el-get-package-effective-library (package-or-source)
  "Return the effective :library of PACKAGE-OR-SOURCE.

See `el-get-sources' for details."
  (let* ((source   (if (or (symbolp package-or-source) (stringp package-or-source))
                       (el-get-package-def package-or-source)
                     package-or-source))
         (package  (if (or (symbolp package-or-source) (stringp package-or-source))
                       (el-get-as-symbol package-or-source)
                     (plist-get source :name)))
         (pkgname  (plist-get source :pkgname))
         (feats    (el-get-as-list (plist-get source :features))))
    (or (plist-get source :library)
        (car feats)
        (if (memq (el-get-package-method source)
                  '(github emacsmirror github-tar github-zip))
            (cdr (el-get-github-parse-user-and-repo package))
          pkgname)
        package)))

(defun el-get-version-to-list (version)
  "Convert VERSION to a standard version list.

Like the builtin `version-to-list', this function accepts a
string.  Unlike the builtin, it will also accept a single number,
which will be wrapped into a single-element list, or a or a list
of numbers, which will be returned unmodified."
  (cond
   ;; String
   ((stringp version)
    (version-to-list version))
   ;; Single number
   ((numberp version)
    (list version))
   ;; List of numbers
   ((and (listp version)
         (null (remove-if 'numberp version)))
    version)
   (t (error "Unrecognized version specification: %S" version))))

(defun el-get-error-unless-required-emacs-version (package-or-source)
  "Raise an error if `emacs-major-version' is less than package's requirement."
  (let* ((pname (el-get-source-name package-or-source))
         (required-version (el-get-package-required-emacs-version
                            package-or-source))
         (required-version-list (el-get-version-to-list required-version)))
    (when (version-list-< (version-to-list emacs-version) required-version-list)
      (error "Package %s requires Emacs version %s or higher, but the current\
 Emacs is only version %s"
             pname required-version emacs-version))))

(defun el-get-envpath-prepend (envname head)
  "Prepend HEAD in colon-separated environment variable ENVNAME.
This is effectively the same as doing the following in shell:
    export ENVNAME=HEAD:$ENVNAME

Use this to modify environment variable such as $PATH or $PYTHONPATH."
  (setenv envname (el-get-envpath-prepend-1 (getenv envname) head)))

(defun el-get-envpath-prepend-1 (paths head)
  "Return \"HEAD:PATHS\" omitting duplicates in it."
  (let ((pplist (split-string (or paths "") path-separator 'omit-nulls)))
    (mapconcat 'identity
               (remove-duplicates (cons head pplist)
                                  :test #'string= :from-end t)
               path-separator)))

(provide 'el-get-recipes)

;;; el-get-recipes.el ends here
