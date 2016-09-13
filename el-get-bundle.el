;;; el-get-bundle.el --- An el-get wrapper

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: https://github.com/tarao/bundle-el/tree/el-get
;; Version: 0.1
;; Keywords: emacs package install compile
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'el-get)
(eval-when-compile (require 'cl))

;; customization

(defgroup el-get-bundle nil "el-get-bundle"
  :group 'el-get)

(defcustom el-get-bundle-byte-compile t
  "t means to automatically byte-compile init forms."
  :type 'boolean
  :group 'el-get-bundle)

(defcustom el-get-bundle-sync t
  "t means to run el-get with the argument 'sync.
The default can be overriden on a per-package basis by adding one of
`:bundle-async [t|nil]'
`:bundle-sync [t|nil]
to the list of keywords that follow
`el-get-bundle PACKAGE|el-get-bundle FEATURE in PACKAGE'."
  :type 'boolean
  :group 'el-get-bundle)

(defcustom el-get-bundle-init-directory
  (expand-file-name "bundle-init/" el-get-dir)
  "Directory to save auto generated init files."
  :type 'directory
  :group 'el-get-bundle)

(defvar el-get-bundle-sources nil)
(defvar el-get-bundle-init-count-alist nil)
(defvar el-get-bundle-init-alist nil)

(defconst el-get-bundle-gist-url-type-plist
  '(http "http://gist.github.com/%s.git"
    https "https://gist.github.com/%s.git"
    git "git://gist.github.com/%s.git"
    ssh "git@gist.github.com:%s.git")
  "Plist mapping Gist types to their URL format strings.")

;; internals

(defsubst el-get-bundle-gist-url (id &optional src)
  (let* ((type (or (plist-get src :url-type) el-get-github-default-url-type))
         (str (or (plist-get el-get-bundle-gist-url-type-plist type)
                  (plist-get el-get-bundle-gist-url-type-plist 'http))))
    (format str id)))

(defsubst el-get-bundle-load-file-el (&optional file)
  (let ((file (or file load-file-name)))
    (and file
         (replace-regexp-in-string "\\.elc$" ".el" (expand-file-name file)))))

(defun el-get-bundle-package-def (src)
  (ignore-errors
    (let (el-get-sources)
      (el-get-package-def (if (listp src) (el-get-source-name src) src)))))
(defalias 'el-get-bundle-defined-p (symbol-function 'el-get-bundle-package-def))

(defun el-get-bundle-guess-type (src)
  (cond
   ((plist-member src :url)
    (let ((url (plist-get src :url)))
      (cond
       ((or (string-match-p "^git:" url) (string-match-p "\\.git$" url))
        'git)
       ((or (string-match-p "^bzr:" url) (string-match-p "^lp:" url))
        'bzr)
       ((string-match-p "^svn:" url)
        'svn)
       ((string-match-p ":pserver:" url)
        'cvs)
       ((string-match-p "ftp://" url)
        'ftp)
       ((or (string-match-p "https?://" url) (string-match-p "\\.el$" url))
        'http))))
   (t 'elpa)))

(defun el-get-bundle-parse-name (sym)
  (let ((spec (split-string (format "%s" sym) ":")) s)
    (when (string= (or (nth 0 spec) "") "github") (setq spec (cdr spec)))
    (cond
     ((and (> (length spec) 2) (string= (car spec) "gist"))
      ;; gist:12345:name
      (let* ((id (nth 1 spec))
             (name (intern (or (nth 2 spec) id)))
             (type 'git) (url (el-get-bundle-gist-url id)))
        (plist-put (plist-put (plist-put s :name name) :type type) :url url)))
     ((> (length spec) 1)
      ;; type:name
      (let ((name (intern (nth 1 spec))) (type (intern (nth 0 spec))))
      (plist-put (plist-put s :name name) :type type)))
     ((= (length (split-string (or (nth 0 spec) "") "/")) 2)
      ;; user/repository
      (let ((name (intern (replace-regexp-in-string "^.*/" "" (nth 0 spec))))
            (type 'github) (pkgname (nth 0 spec)))
        (plist-put (plist-put (plist-put s :name name) :type type)
                   :pkgname pkgname)))
     (t (plist-put s :name sym)))))

(defun el-get-bundle-init-id (callsite)
  (let ((pair (assoc callsite el-get-bundle-init-count-alist)))
    (if pair
        (setcdr pair (1+ (cdr pair)))
      (push (cons callsite 1) el-get-bundle-init-count-alist)
      1)))

(defun el-get-bundle-load-init (el)
  (el-get-byte-compile-file el byte-compile-warnings)
  (load (expand-file-name (file-name-sans-extension el))))

(defun el-get-bundle-make-init (src)
  (when (and el-get-bundle-byte-compile
             (plist-get src :after)
             load-file-name
             (ignore-errors
               (or (file-exists-p el-get-bundle-init-directory)
                   (make-directory el-get-bundle-init-directory t) t)))
    (let* ((path (expand-file-name (convert-standard-filename load-file-name)))
           (path (file-name-sans-extension path))
           (callsite (replace-regexp-in-string "[^a-zA-Z0-9._-]" "_" path))
           (package (plist-get src :name))
           (id (el-get-bundle-init-id path))
           (init-file (concat el-get-bundle-init-directory
                              (format "%s-%d_%s" callsite id package)))
           (el (concat init-file ".el"))
           (form (plist-get src :after))
           (loader load-file-name))
      (let ((loader-el (concat (file-name-sans-extension loader) ".el")))
        (when (and (string-match-p "\\.elc$" loader)
                   (file-exists-p loader-el))
          (setq loader loader-el)))
      ;; remember the package-initializer relation
      (let* ((pair (assoc package el-get-bundle-init-alist))
             (inits (cdr pair)))
        (if pair
            (setcdr pair (add-to-list 'inits init-file))
          (push (cons package (list init-file)) el-get-bundle-init-alist)))
      ;; generate .el file
      (when (or (not (file-exists-p el))
                (file-newer-than-file-p loader el))
        (with-temp-buffer
          (if (listp form)
              (dolist (exp form) (pp exp (current-buffer)))
            (pp form (current-buffer)))
          (write-region nil nil el)))

      ;; loader
      `((el-get-bundle-load-init ,el)))))

;;;###autoload
(defun el-get-bundle-el-get (src sync)
  (let ((package (plist-get src :name)) (def (el-get-bundle-package-def src))
        (fs (plist-get src :features))
        (ds (plist-get src :depends)))
    ;; merge features
    (when (plist-member def :features)
      (let ((old (el-get-as-list (plist-get def :features))))
        (dolist (f old) (add-to-list 'fs f))
        (setq src (plist-put src :features fs))))
    ;; merge depends
    (when (plist-member def :depends)
      (let ((old (el-get-as-list (plist-get def :depends))))
        (dolist (d old) (add-to-list 'ds d))
        (setq src (plist-put src :depends ds))))

    ;; merge src with the oriiginal definition
    (setq def (let ((el-get-sources (list src)))
                (el-get-package-def (el-get-source-name src))))

    ;; entering password via process-filter only works in async mode
    (when (or (and (eq (plist-get def :type) 'cvs)
                   (eq (plist-get def :options) 'login)
                   (not (string-match-p "^:pserver:.*:.*@.*:.*$"
                                        (or (plist-get def :url) ""))))
              (eq (plist-get def :type) 'apt)
              (eq (plist-get def :type) 'fink)
              (eq (plist-get def :type) 'pacman))
      (setq sync nil))

    ;; byte-compile :after script
    (let ((form (or (el-get-bundle-make-init def) (plist-get def :after))))
      (when form
        (setq def (plist-put def :after `(progn ,@form)))))

    (let ((toplevel (null el-get-bundle-sources)))
      ;; save sources to global variable
      (when toplevel (setq el-get-bundle-sources el-get-sources))
      (add-to-list 'el-get-bundle-sources def)

      ;; get
      (prog1 (let ((el-get-sources el-get-bundle-sources))
               (el-get sync package))
        ;; prevent :after from running twice
        (plist-put def :after nil)

        ;; apply changes of sources to `el-get-sources' variable
        (when toplevel
          (setq el-get-sources el-get-bundle-sources
                el-get-bundle-sources nil))))))

(defun el-get-bundle-post-update (package)
  "Post update process for PACKAGE.

Invalidate byte-compiled package initialization forms of
PACKAGE (for future recompilation)."
  (let ((inits (assoc package el-get-bundle-init-alist)))
    (dolist (name (cdr (assoc package el-get-bundle-init-alist)))
      (let ((file (concat name ".elc")))
        (when (file-exists-p file)
          (delete-file file))))))
(add-hook 'el-get-post-update-hooks #'el-get-bundle-post-update)

(defun el-get-bundle-clear-init-count (callsite)
  ;; clear the number of configurations so that the number starts with
  ;; 1 next time CALLSITE is loaded
  (let ((key (file-name-sans-extension (expand-file-name callsite))))
    (setq el-get-bundle-init-count-alist
          (delq (assoc key el-get-bundle-init-count-alist)
                el-get-bundle-init-count-alist))))
(add-hook 'after-load-functions #'el-get-bundle-clear-init-count)

;; commands

;;;###autoload
(defmacro el-get-bundle (package &rest form)
  "Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The property list may include the keyword
`:bundle-sync' with a value of either `t' or `nil' to request that
`el-get-bundle' invoke `el-get' synchronously (respectively asynchronously).
The keyword `:bundle-async' is the inverse of `:bundle-sync'.
\(Note that the request to run el-get synchronously may not be respected in all
circumstances: see the definition of `el-get-bundle-el-get' for details.)
The FORM after the property list is treated as initialization code,
which is actually an `:after' property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil."
  (declare (indent defun) (debug t))
  (let* ((package (or (and (listp package) (nth 1 package)) package))
         (src (el-get-bundle-parse-name package))
         (sync el-get-bundle-sync) require)
    ;; set parsed name
    (setq package (plist-get src :name))
    ;; (el-get-bundle FEATURE in PACKAGE ...) form
    (when (eq (nth 0 form) 'in)
      (let* ((name (nth 1 form))
             (name (or (and (listp name) (nth 1 name)) name)))
        (setq src (el-get-bundle-parse-name name)))
      (setq form (nthcdr 2 form) require t))
    ;; parse keywords
    (while (keywordp (nth 0 form))
      (case (nth 0 form)
        (:bundle-sync (setq sync (nth 1 form)))
        (:bundle-async (setq sync (not (nth 1 form))))
        (t (setq src (plist-put src (nth 0 form) (nth 1 form)))))
      (setq form (cdr-safe (cdr form))))
    ;; put default type
    (unless (or (plist-member src :type) (el-get-bundle-defined-p src))
      (setq src (plist-put src :type (el-get-bundle-guess-type src))))
    ;; features
    (when (plist-member src :features)
      (let ((fs (el-get-as-list (plist-get src :features))))
        (setq src (plist-put src :features fs))))
    (when (and require (or (not (plist-member src :features))
                           (plist-get src :features)))
      ;; put the feature into the features list
      (let ((fs (plist-get src :features)))
        (add-to-list 'fs package)
        (setq src (plist-put src :features fs))))
    ;; init script
    (setq src (plist-put src :after form))

    `(el-get-bundle-el-get ',src ',(when sync 'sync))))

;;;###autoload
(defmacro el-get-bundle! (package &rest args)
  "Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required."
  (declare (indent defun) (debug t))
  (if (eq (nth 0 args) 'in)
      `(el-get-bundle ,package ,@args)
    (let* ((package (or (and (listp package) (nth 1 package)) package))
           (name (plist-get (el-get-bundle-parse-name package) :name)))
      `(el-get-bundle ,name ,@(list* 'in package args)))))

(provide 'el-get-bundle)
;;; el-get-bundle.el ends here
