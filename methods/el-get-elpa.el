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

(require 'el-get-core)
(require 'el-get-recipes)
(require 'package nil t)

(declare-function el-get-package-is-installed "el-get" (package))

;;; package.el compat functions
(eval-and-compile
  ;; Use the 24.4 accessor names
  (unless (fboundp 'package-desc-summary)
    (defalias 'package-desc-summary 'package-desc-doc))
  (unless (fboundp 'package-desc-version)
    (defalias 'package-desc-version 'package-desc-vers))
  ;; Introduced in 24.4
  (unless (fboundp 'package-desc-archive)
    (defun package-desc-archive (desc)
      (aref desc (1- (length desc)))))

  ;; In 24.4 each package has a *list* of descriptors, pretend it's so
  ;; in 24.3 and below as well.
  (defun el-get-elpa-descs (alist-elem)
    "Return a list of descriptors for PKG from ALIST-ELEM.

ALIST-ELEM should be an element from `package-alist' or
`package-archive-contents'."
    (el-get-as-list (cdr alist-elem)))

  (defun el-get-elpa-delete-package (pkg)
    "A wrapper for `package-delete', deletes all versions."
    (let ((descs (cdr (assq pkg package-alist))))
      (if (listp descs)
          ;; 24.4+ case, we have a list of descriptors that we can
          ;; call `package-delete' on.
          (mapc #'package-delete descs)
        ;; Otherwise, just delete the package directory.
        (delete-directory (el-get-elpa-package-directory pkg) 'recursive))))

  (defun el-get-elpa-package-id (pkg)
    "A compat utility function."
    ;; In 24.4+ we have a list of descs, earlier versions just use the
    ;; name (a symbol) to specify the package.
    (let* ((descs (cdr (assq pkg package-archive-contents))))
      (cond
       ((consp descs) (car descs))
       ((null descs) (error "Couln't find package `%s'" pkg))
       (t pkg))))

  (defun el-get-elpa-package-archive-base (pkg)
    "Compat wrapper for `package-archive-base'."
    (package-archive-base (el-get-elpa-package-id pkg)))

  (defun el-get-elpa-install-package (pkg have-deps-p)
    "A wrapper for package.el installion.

Installs the 1st available version. If HAVE-DEPS-P skip
package.el's dependency computations."
    (let ((to-install (el-get-elpa-package-id pkg)))
      (if have-deps-p
          (package-download-transaction (list to-install))
        (package-install to-install)))))

(defcustom el-get-elpa-install-hook nil
  "Hook run after ELPA package install."
  :group 'el-get
  :type 'hook)

(defcustom el-get-elpa-remove-hook nil
  "Hook run after ELPA package remove."
  :group 'el-get
  :type 'hook)

(defun el-get-elpa-package-directory (package)
  "Return the directory where ELPA stores PACKAGE, or nil if
PACKAGE isn't currently installed by ELPA."
  (require 'package)
  ;; package directories are named <package>-<version>.
  (let* ((pname (el-get-as-string package))
         (pregex (concat "\\`" (regexp-quote pname) "-"))
         (version-offset (+ (length pname) 1)))
    (catch 'dir
     (loop for pkg-base-dir in (cons package-user-dir
                                     (when (boundp 'package-directory-list)
                                       package-directory-list))
           when (file-directory-p pkg-base-dir)
           do
           (loop for pkg-dir in (directory-files pkg-base-dir nil pregex)
                 if (ignore-errors
                      (version-to-list (substring pkg-dir version-offset)))
                 do (throw 'dir (expand-file-name pkg-dir pkg-base-dir))))
     nil)))

(defun el-get-elpa-package-repo (package)
  "Get the ELPA repository cons cell for PACKAGE.

The cons cell has the form (NAME . URL). See `package-archives'.
If the package source only specifies a URL, the URL will be used
for NAME as well.

If PACKAGE's `:type' is not \"elpa\", or no repo is specified in
the recipe, then return nil."
  (let* ((source (el-get-package-def package))
         (type   (el-get-package-type source))
         (elpa-repo (plist-get source :repo)))
    (when (and (eq type 'elpa) elpa-repo)
      (cond ((stringp elpa-repo)
             (cons elpa-repo elpa-repo))
            ((consp elpa-repo)
             (if (and (stringp (car elpa-repo))
                      (stringp (cdr elpa-repo)))
                 elpa-repo
               (error "Invalid elpa repo spec: %s" elpa-repo)))
            (t
             (error "Invalid elpa repo spec: %s" elpa-repo))))))

(defun el-get-elpa-symlink-package (package)
  "ln -s ../elpa/<package> ~/.emacs.d/el-get/<package>"
  (let* ((package  (el-get-as-string package))
         (pkg-dir (el-get-elpa-package-directory package))
         (elpa-dir (if pkg-dir
                       (file-relative-name pkg-dir el-get-dir)
                     (error "No package directory for `%s' found" package))))
    (unless (el-get-package-exists-p package)
      ;; better style would be to check for (fboundp 'make-symbolic-link) but
      ;; that would be true on Vista, where by default only administrator is
      ;; granted to use the feature --- so hardcode those systems out
      (if (memq system-type '(ms-dos windows-nt))
          ;; in windows, we have to actually copy the directories,
          ;; since symlink is not exactly reliable on those systems
          (copy-directory (el-get-elpa-package-directory package)
                          (file-name-as-directory (expand-file-name package el-get-dir)))
        (let ((default-directory el-get-dir))
          (make-symbolic-link elpa-dir package))))))

(eval-when-compile
  ;; `condition-case-unless-debug' was introduced in 24.1, but was
  ;; actually added in 23.1 as `condition-case-no-debug'
  (unless (fboundp 'condition-case-unless-debug)
    (defalias 'condition-case-unless-debug 'condition-case-no-debug)))

(defun el-get-elpa-install (package url post-install-fun)
  "Ask elpa to install given PACKAGE."
  (let* ((have-deps-p (plist-member (el-get-package-def package) :depends))
         (elpa-dir (el-get-elpa-package-directory package))
         (elpa-repo (el-get-elpa-package-repo package))
         ;; Indicates new archive requiring to download its archive-contents
         (elpa-new-repo (when (and elpa-repo)
                          (unless (rassoc (cdr-safe elpa-repo)
                                          (bound-and-true-p package-archives))
                            elpa-repo)))
         ;; Set `package-archive-base' to elpa-repo for old package.el
         (package-archive-base (or (cdr-safe elpa-repo)
                                   (bound-and-true-p package-archive-base)))
         ;; Prepend elpa-repo to `package-archives' for new package.el
         (package-archives (append (when elpa-repo (list elpa-repo))
                                   (when (boundp 'package-archives) package-archives))))

    (unless (and elpa-dir (file-directory-p elpa-dir))
      ;; package-install does these only for interactive calls
      (unless package--initialized
        (package-initialize t))
      (cond ((not package-archive-contents)
             (package-refresh-contents))
            (elpa-new-repo
             (condition-case-unless-debug nil
                 (package--download-one-archive elpa-new-repo "archive-contents")
               (error (message "Failed to download `archive-contents' for `%s' from `%s'."
                               (car elpa-new-repo)
                               (cdr elpa-new-repo))))
             (package-read-all-archive-contents)))
      ;; We can only get the url after `package-archive-contents' is
      ;; initialized.
      (setq url (or (cdr elpa-repo)
                    (el-get-elpa-package-archive-base package)))
      (el-get-insecure-check package url)

      ;; TODO: should we refresh and retry once if package-install fails?
      ;; package-install generates autoloads, byte compiles
      (let (emacs-lisp-mode-hook fundamental-mode-hook prog-mode-hook)
        (el-get-elpa-install-package (el-get-as-symbol package) have-deps-p)))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package))
  (funcall post-install-fun package))

(defun el-get-elpa-update-available-p (package)
  "Returns t if PACKAGE has an update available in ELPA."
  (assert (el-get-package-is-installed package) nil
          (format "Cannot update non-installed ELPA package %s" package))
  (let* ((installed-version
          (package-desc-version (car (el-get-elpa-descs (assq package package-alist)))))
         (available-packages
          (el-get-elpa-descs (assq package package-archive-contents))))
    ;; Emacs 24.4 keeps lists of available packages. `package-alist'
    ;; is sorted by version, but `package-archive-contents' is not, so
    ;; we should loop through it.
    (some (lambda (pkg)
            (version-list-< installed-version
                            (package-desc-version pkg)))
          available-packages)))

(defvar el-get-elpa-do-refresh t
  "Whether to call `package-refresh-contents' during `el-get-elpa-update'.

Let-bind this variable to `once' around many `el-get-elpa-update'
calls to ensure `package-refresh-contents' is only called the
first time.")

(defun el-get-elpa-update (package url post-update-fun)
  "Ask elpa to update given PACKAGE."
  (unless package--initialized
    (package-initialize t))
  (el-get-insecure-check package url)
  (when el-get-elpa-do-refresh
   (package-refresh-contents)
   (when (eq el-get-elpa-do-refresh 'once)
     (setq el-get-elpa-do-refresh nil)))
  (when (el-get-elpa-update-available-p package)
    (el-get-elpa-remove package url nil)
    (el-get-elpa-install-package
     (el-get-as-symbol package)
     (plist-member (el-get-package-def package) :depends))
    ;; in windows, we don't have real symlinks, so its better to remove
    ;; the directory and copy everything again
    (when (memq system-type '(ms-dos windows-nt))
      (delete-directory (el-get-elpa-package-directory package) t))
    (el-get-elpa-symlink-package package))
  (funcall post-update-fun package))

(defun el-get-elpa-remove (package url post-remove-fun)
  "Remove the right directory where ELPA did install the package."
  ;; This just removes the symlink from el-get's directory. Look in
  ;; `el-get-elpa-post-remove' for the piece that actually removes the
  ;; package.
  (el-get-rmdir package url post-remove-fun))

(defun el-get-elpa-post-remove (package)
  "Do remove the ELPA bits for package, now"
  (el-get-elpa-delete-package package))

(add-hook 'el-get-elpa-remove-hook 'el-get-elpa-post-remove)

(defun el-get-elpa-guess-website (package)
  "Guess website for elpa PACKAGE."
  (let* ((repo (el-get-elpa-package-repo package))
         (repo-name (car repo))
         (repo-url (cdr repo))
         (package (el-get-as-string package)))
    (cond
     ((or (not repo)
          (string= "gnu" repo-name)
          (string-match-p "elpa\\.gnu\\.org" repo-url))
      (concat "http://elpa.gnu.org/packages/" package))
     ((or (string= "marmalade" repo-name)
          (string-match-p "marmalade-repo\\.org" repo-url))
      (concat "http://marmalade-repo.org/packages/" package))
     ((or (string= "melpa" repo-name)
          (string-match-p "melpa.org" repo-url))
      (concat "http://melpa.org/#" package)))))

(el-get-register-method :elpa
  :install #'el-get-elpa-install
  :update #'el-get-elpa-update
  :remove #'el-get-elpa-remove
  :install-hook 'el-get-elpa-install-hook
  :remove-hook 'el-get-elpa-remove-hook
  :guess-website #'el-get-elpa-guess-website)

;;;
;;; Functions to maintain a local recipe list from ELPA
;;;

;;;###autoload
(defun el-get-elpa-build-local-recipes (&optional target-dir do-not-update)
  "retrieves list of ELPA packages and turn them to local recipe set.
TARGET-DIR is the target directory
DO-NOT-UPDATE will not update the package archive contents before running this."
  (interactive)
  (let ((target-dir (or target-dir
                        (car command-line-args-left)
                        el-get-recipe-path-elpa))
        (coding-system-for-write 'utf-8)
        (pkgs nil))

    (unless (file-directory-p target-dir)
      (make-directory target-dir 'recursive))

    (when (or (not package-archive-contents)
              (and package-archive-contents
                   (not do-not-update)))
      (package-refresh-contents))
    (setq pkgs package-archive-contents)

    (with-temp-buffer
      (dotimes-with-progress-reporter (_ (length package-archive-contents))
          "Generating recipes from package.el descriptors..."
        (let* ((pkg         (pop pkgs))
               (package     (format "%s" (car pkg)))
               (pkg-desc    (car (el-get-elpa-descs pkg)))
               (description (package-desc-summary pkg-desc))
               (pkg-deps    (package-desc-reqs pkg-desc))
               (depends     (remq 'emacs (mapcar #'car pkg-deps)))
               (emacs-dep   (assq 'emacs pkg-deps))
               (repo        (assoc (package-desc-archive pkg-desc)
                                   package-archives)))
          (erase-buffer)
          (insert
           (format
            "(:name %s\n:auto-generated t\n:type elpa\n:description %S\n:repo %S\n"
            package description repo))
          (when depends
            (insert (format ":depends %s\n" depends)))
          (when emacs-dep
            (insert (format ":minimum-emacs-version %s\n"
                            (cadr emacs-dep))))
          (insert ")")
          (goto-char (point-min))
          (write-region nil nil
                        (expand-file-name (concat package ".rcp") target-dir)
                        nil 0))))))

(provide 'el-get-elpa)
