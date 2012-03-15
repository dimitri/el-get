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
;;     Please see the README.asciidoc file from the same distribution

;;; Commentary:
;;
;; el-get-recipes provides the API to manage the el-get recipes.
;;

;; el-get-core provides basic el-get API, intended for developpers of el-get
;; and its methods.  See the methods directory for implementation of them.
;;

(require 'el-get-core)
(require 'el-get-byte-compile)

(defcustom el-get-recipe-path-emacswiki
  (concat (file-name-directory el-get-dir) "el-get/recipes/emacswiki/")
  "Define where to keep a local copy of emacswiki recipes"
  :group 'el-get
  :type 'directory)

(defcustom el-get-recipe-path-elpa
  (concat (file-name-directory el-get-dir) "el-get/recipes/elpa/")
  "Define where to keep a local copy of elpa recipes"
  :group 'el-get
  :type 'directory)


(defcustom el-get-recipe-path
  (list (concat (file-name-directory el-get-script) "recipes")
       el-get-recipe-path-elpa
	el-get-recipe-path-emacswiki)
  "Define where to look for the recipes, that's a list of directories"
  :group 'el-get
  :type '(repeat (directory)))

(defcustom el-get-user-package-directory nil
  "Define where to look for init-pkgname.el configurations. Disabled if nil."
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
	   (compiled-init-file (concat (file-name-sans-extension package-init-file) ".elc")))
      (when (file-exists-p package-init-file)
	(el-get-byte-compile-file package-init-file)
	(el-get-verbose-message "el-get: load %S" compiled-init-file)
	(load compiled-init-file 'noerror)))))

(defun el-get-recipe-dirs ()
  "Return the elements of el-get-recipe-path that actually exist.

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
(defun el-get-read-recipe-file (filename)
  "Read given filename and return its content (a valid form is expected)"
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    ((debug error)
     (error "Error reading recipe %s: %S" filename err))))

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
	(el-get-read-recipe-file filename)
      (error "el-get can not find a recipe for package \"%s\"." package))))

(defun el-get-read-all-recipes ()
  "Return the list of all the recipes, formatted like `el-get-sources'.

Only consider any given recipe only once even if present in
multiple dirs from `el-get-recipe-path'. The first recipe found
is the one considered.  We first look in `el-get-sources' then in
each directory listed in `el-get-recipe-path' in order."
  (let ((packages (mapcar 'el-get-source-name el-get-sources)))
    (append
     el-get-sources
     (loop for dir in (el-get-recipe-dirs)
	   nconc (loop for recipe in (directory-files dir nil "^[^.].*\.\\(rcp\\|el\\)$")
		       for filename = (concat (file-name-as-directory dir) recipe)
		       for package = (file-name-sans-extension (file-name-nondirectory recipe))
		       unless (member package packages)
		       do (push package packages)
                       and collect (ignore-errors (el-get-read-recipe-file filename)))))))

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
	   (loop with def = (el-get-read-recipe package)
		 for (prop override) on source by 'cddr
		 do (plist-put def prop override)
		 finally return def))

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

    (if (and builtin (>= emacs-major-version builtin))
        'builtin
      (plist-get def :type))))

(defalias 'el-get-package-type #'el-get-package-method)

(defun el-get-package-types-alist (statuses &rest types)
  "Return an alist of package names that are of given types.

Only consider packages whose status is `member' of STATUSES,
which defaults to installed, required and removed.  Example:

  (el-get-package-types-alist \"installed\" 'http 'cvs)
"
  (loop for src in (apply 'el-get-list-package-names-with-status
			  (cond ((stringp statuses) (list statuses))
				((null statuses) '("installed" "required" "removed"))
				(t statuses)))
	for name = (el-get-as-symbol src)
	for type = (el-get-package-type name)
	when (or (null types) (memq 'all types) (memq type types))
	collect (cons name type)))

(provide 'el-get-recipes)
