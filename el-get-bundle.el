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

(defcustom el-get-bundle-init-directory
  (expand-file-name "bundle-init/" el-get-dir)
  "Directory to save auto generated init files."
  :type 'directory
  :group 'el-get-bundle)

(defcustom el-get-bundle-reload-user-init-file t
  "Reload `user-init-file' when a package is updated."
  :type 'boolean
  :group 'el-get-bundle)

(defvar el-get-bundle-sources nil)
(defvar el-get-bundle-init-count-alist nil)
(defvar el-get-bundle-init-alist nil)
(defvar el-get-bundle-updates nil)

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

(defun el-get-bundle-init-id (&rest args)
  (let* ((key (mapconcat #'(lambda (x) (format "%s" x)) args ";"))
         (pair (assoc key el-get-bundle-init-count-alist)))
    (if pair
        (setcdr pair (1+ (cdr pair)))
      (push (cons key 1) el-get-bundle-init-count-alist)
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
    (let* ((path (file-name-sans-extension (expand-file-name load-file-name)))
           (path (split-string path "/"))
           (callsite (mapconcat #'identity path "_"))
           (package (plist-get src :name))
           (id (el-get-bundle-init-id package callsite))
           (init-file (concat el-get-bundle-init-directory
                              (format "%s_%s-%d" package callsite id)))
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
(defun el-get-bundle-el-get (src)
  (let ((package (plist-get src :name)) (def (el-get-bundle-package-def src))
        (fs (plist-get src :features)) (sync 'sync))
    ;; merge features
    (when (plist-member def :features)
      (let* ((old (plist-get def :features))
             (old (or (and (listp old) old) (list old))))
        (dolist (f old) (add-to-list 'fs f))
        (setq src (plist-put src :features fs))))
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
PACKAGE (for future recompilation).  If the PACKAGE is the last
file updated by an update command and
`el-get-bundle-reload-user-init-file' is non-nil, then
`user-init-file' is loaded again."
  (let ((inits (assoc package el-get-bundle-init-alist)))
    (dolist (name (cdr (assoc package el-get-bundle-init-alist)))
      (let ((file (concat name ".elc")))
        (when (file-exists-p file)
          (delete-file file)))))
  (when el-get-bundle-updates
    (setq el-get-bundle-updates (delq package el-get-bundle-updates))
    (when (and (null el-get-bundle-updates) el-get-bundle-reload-user-init-file)
      (setq el-get-bundle-init-count-alist nil
            el-get-bundle-init-alist nil)
      (when (stringp user-init-file)
        (load user-init-file)
        (run-hooks 'after-init-hook)))))
(add-hook 'el-get-post-update-hooks #'el-get-bundle-post-update)

;; commands

;;;###autoload
(defmacro el-get-bundle (feature &rest form)
  "Install FEATURE and run init script specified by FORM.

FORM may be started with a property list. In that case, the
property list is pushed to `el-get-sources'.

The rest of FORM is evaluated after FEATURE is loaded."
  (declare (indent defun) (debug t))
  (let* ((feature (or (and (listp feature) (nth 1 feature)) feature))
         (src (el-get-bundle-parse-name feature)) require)
    ;; set parsed name
    (setq feature (plist-get src :name))
    ;; (el-get-bundle FEATURE in PACKAGE ...) form
    (when (eq (nth 0 form) 'in)
      (let* ((name (nth 1 form))
             (name (or (and (listp name) (nth 1 name)) name)))
        (setq src (el-get-bundle-parse-name name)))
      (setq form (nthcdr 2 form) require t))
    ;; parse keywords
    (while (keywordp (nth 0 form))
      (setq src (plist-put src (nth 0 form) (nth 1 form))
            form (cdr-safe (cdr form))))
    ;; put default type
    (unless (or (plist-member src :type) (el-get-bundle-defined-p src))
      (setq src (plist-put src :type (el-get-bundle-guess-type src))))
    ;; features
    (when (plist-member src :features)
      (let* ((fs (plist-get src :features))
             (fs (or (and (listp fs) fs) (list fs))))
        (setq src (plist-put src :features fs))))
    (when (and require (or (not (plist-member src :features))
                           (plist-get src :features)))
      ;; put the feature into the features list
      (let ((fs (plist-get src :features)))
        (add-to-list 'fs feature)
        (setq src (plist-put src :features fs))))
    ;; init script
    (setq src (plist-put src :after form))

    `(el-get-bundle-el-get ',src)))

;;;###autoload
(defmacro el-get-bundle! (feature &rest args)
  "Install FEATURE and run init script.
It is the same as `el-get-bundle' except that FEATURE is explicitly
required."
  (declare (indent defun) (debug t))
  (if (eq (nth 0 args) 'in)
      `(el-get-bundle ,feature ,@args)
    (let* ((feature (or (and (listp feature) (nth 1 feature)) feature))
           (name (plist-get (el-get-bundle-parse-name feature) :name)))
      `(el-get-bundle ,name ,@(list* 'in feature args)))))

;;;###autoload
(defun el-get-bundle-update (&rest packages)
  "Update PACKAGES.
If PACKAGES is nil, then update all installed packages.  If
`el-get-bundle-reload-user-init-file' is non-nil, then `user-init-file'
is reloaded after all the updates."
  (interactive "SPackage: ")
  (setq el-get-bundle-updates packages)
  (if packages
      (mapc #'el-get-update packages)
    (setq el-get-bundle-updates
          (el-get-list-package-names-with-status "installed"))
    (el-get-update-all t)))

;;;###autoload
(defun el-get-bundle-update-all ()
  "Update all installed packages.
If `el-get-bundle-reload-user-init-file' is non-nil, then
`user-init-file' is reloaded after all the updates."
  (interactive)
  (el-get-bundle-update))

(provide 'el-get-bundle)
;;; el-get-bundle.el ends here
