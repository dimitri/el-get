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
;; pretend these old functions exist to keep byte compiler in 24.4 quiet
(declare-function package-desc-doc "package" (desc))
(declare-function package-desc-vers "package" (desc))

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
         (version-offset (+ (length pname) 1)))
    (loop for pkg-base-dir in (cons package-user-dir
                                    (when (boundp 'package-directory-list)
                                      package-directory-list))
          with dir = nil
          when (file-directory-p pkg-base-dir)
          do
          (setq dir
                (loop for pkg-dir in (directory-files
                                      pkg-base-dir nil
                                      (concat "\\`" (regexp-quote pname) "-"))
                      if (ignore-errors
                           (version-to-list (substring pkg-dir version-offset)))
                      return (expand-file-name pkg-dir pkg-base-dir)))
          and when dir return dir)))

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
         (elpa-dir (file-relative-name
                    (el-get-elpa-package-directory package) el-get-dir)))
    (unless (el-get-package-exists-p package)
      ;; better style would be to check for (fboundp 'make-symbolic-link) but
      ;; that would be true on Vista, where by default only administrator is
      ;; granted to use the feature --- so hardcode those systems out
      (if (memq system-type '(ms-dos windows-nt))
          ;; in windows, we have to actually copy the directories,
          ;; since symlink is not exactly reliable on those systems
          (copy-directory (el-get-elpa-package-directory package)
                          (file-name-as-directory (expand-file-name package el-get-dir)))
        (message "%s"
                 (shell-command
                  (format "cd %s && ln -s \"%s\" \"%s\"" el-get-dir elpa-dir package)))))))

(eval-when-compile
  ;; `condition-case-unless-debug' was introduced in 24.1, but was
  ;; actually added in 23.1 as `condition-case-no-debug'
  (unless (fboundp 'condition-case-unless-debug)
    (defalias 'condition-case-unless-debug 'condition-case-no-debug)))

(defun el-get-elpa-install (package url post-install-fun)
  "Ask elpa to install given PACKAGE."
  (let* ((elpa-dir (el-get-elpa-package-directory package))
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
      ;; TODO: should we refresh and retry once if package-install fails?
      ;; package-install generates autoloads, byte compiles
      (let (emacs-lisp-mode-hook fundamental-mode-hook prog-mode-hook)
        (package-install (el-get-as-symbol package))))
    ;; we symlink even when the package already is installed because it's
    ;; not an error to have installed ELPA packages before using el-get, and
    ;; that will register them
    (el-get-elpa-symlink-package package))
  (funcall post-install-fun package))

(defun el-get-elpa-update-available-p (package)
  "Returns t if PACKAGE has an update available in ELPA."
  (assert (el-get-package-is-installed package) nil
          (format "Cannot update non-installed ELPA package %s" package))
  (let* ((pkg-version
          (if (fboundp 'package-desc-version) ;; new in Emacs 24.4
              #'package-desc-version #'package-desc-vers))
         (installed-version
          (funcall pkg-version (car (el-get-as-list (cdr (assq package package-alist))))))
         (available-packages
          (el-get-as-list (cdr (assq package package-archive-contents)))))
    ;; Emacs 24.4 keeps lists of available packages. `package-alist'
    ;; is sorted by version, but `package-archive-contents' is not, so
    ;; we should loop through it.
    (some (lambda (pkg)
            (version-list-< installed-version
                            (funcall pkg-version pkg)))
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
  (when el-get-elpa-do-refresh
   (package-refresh-contents)
   (when (eq el-get-elpa-do-refresh 'once)
     (setq el-get-elpa-do-refresh nil)))
  (when (el-get-elpa-update-available-p package)
    (el-get-elpa-remove package url nil)
    (package-install (el-get-as-symbol package))
    ;; in windows, we don't have real symlinks, so its better to remove
    ;; the directory and copy everything again
    (when (memq system-type '(ms-dos windows-nt))
      (delete-directory (el-get-elpa-package-directory package) t)
      (el-get-elpa-symlink-package package)))
  (funcall post-update-fun package))

(defun el-get-elpa-remove (package url post-remove-fun)
  "Remove the right directory where ELPA did install the package."
  ;; This just removes the symlink from el-get's directory. Look in
  ;; `el-get-elpa-post-remove' for the piece that actually removes the
  ;; package.
  (el-get-rmdir package url post-remove-fun))

(defun el-get-elpa-post-remove (package)
  "Do remove the ELPA bits for package, now"
  (let ((p-elpa-dir (el-get-elpa-package-directory package)))
    (if p-elpa-dir
        (dired-delete-file p-elpa-dir 'always)
      (message "el-get: could not find ELPA dir for %s." package))))

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
          (string-match-p "melpa.milkbox.net" repo-url))
      (concat "http://melpa.milkbox.net/#" package)))))

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
        pkg package description)

    (when (or (not package-archive-contents)
              (and package-archive-contents
                   (not do-not-update)))
      (package-refresh-contents))

    (unless (file-directory-p target-dir)
      (make-directory target-dir 'recursive))

    (mapc (lambda (pkg)
            (let* ((package     (format "%s" (car pkg)))
                   (pkg-desc    (car (el-get-as-list (cdr pkg))))
                   (get-summary (if (fboundp #'package-desc-summary)
                                    #'package-desc-summary #'package-desc-doc))
                   (description (funcall get-summary pkg-desc))
                   (pkg-deps    (package-desc-reqs pkg-desc))
                   (depends     (remq 'emacs (mapcar #'car pkg-deps)))
                   (emacs-dep   (assq 'emacs pkg-deps))
                   (repo
                    (assoc (aref pkg-desc (- (length pkg-desc) 1))
                           package-archives)))
              (with-temp-file (expand-file-name (concat package ".rcp")
                                                target-dir)
                (message "%s:%s" package description)
                (insert
                 (format
                  "(:name %s\n:auto-generated t\n:type elpa\n:description \"%s\"\n:repo %S\n"
                  package description repo))
                (when depends
                  (insert (format ":depends %s\n" depends)))
                (when emacs-dep
                  (insert (format ":minimum-emacs-version %s\n"
                                  (cadr emacs-dep))))
                (insert ")")
                (indent-region (point-min) (point-max)))))
          package-archive-contents)))

(provide 'el-get-elpa)
