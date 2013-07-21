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
(require 'package nil t)

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
  (let* ((pname (format "%s" package))  ; easy way to cope with symbols etc.

     (ls-command (if (memq system-type '(ms-dos windows-nt)) "dir /B " "ls -1 "))

	 (l
	  ;; we use try-completion to find the realname of the directory
	  ;; ELPA used, and this wants an alist, we trick ls -i -1 into
	  ;; that.
	  (mapcar 'split-string
		  (split-string
		   (shell-command-to-string
		    (concat
		     ls-command
                     (shell-quote-argument
                      (expand-file-name
                       (file-name-as-directory package-user-dir))))))))

	 (realname (try-completion pname l)))

    (if realname (concat (file-name-as-directory package-user-dir) realname)
      realname)))

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
	  ;; the symlink is a docs/debug feature, mkdir is ok enough
	  (make-directory (el-get-package-directory package))
	(message "%s"
		 (shell-command
		  (format "cd %s && ln -s \"%s\" \"%s\"" el-get-dir elpa-dir package)))))))

(defun el-get-elpa-install (package url post-install-fun)
  "Ask elpa to install given PACKAGE."
  (let* ((elpa-dir (el-get-elpa-package-directory package))
         (elpa-repo (el-get-elpa-package-repo package))
         ;; Set `package-archive-base' to elpa-repo for old package.el
         (package-archive-base (or (cdr-safe elpa-repo)
                                   (bound-and-true-p package-archive-base)))
         ;; Prepend elpa-repo to `package-archives' for new package.el
         (package-archives (append (when elpa-repo (list elpa-repo))
                                   (when (boundp 'package-archives) package-archives))))
    (unless (and elpa-dir (file-directory-p elpa-dir))
      ;; Make sure we have got *some* kind of record of the package archive.
      ;; TODO: should we refresh and retry once if package-install fails?
      (let ((p (if (fboundp 'package-read-all-archive-contents)
		   (package-read-all-archive-contents) ; version from emacs24
		 (package-read-archive-contents)))     ; old version
            ;; package-install generates autoloads, byte compiles
            emacs-lisp-mode-hook fundamental-mode-hook prog-mode-hook)
	(unless p
	  (package-refresh-contents)))
      (package-install (el-get-as-symbol package)))
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
              #'(lambda (pkg) (package-desc-version (car pkg)))
            #'package-desc-vers))
         (installed-version
          (funcall pkg-version (cdr (assq package package-alist))))
         (available-version
          (funcall pkg-version (cdr (assq package package-archive-contents)))))
    (version-list-< installed-version available-version)))

(defun el-get-elpa-update (package url post-update-fun)
  "Ask elpa to update given PACKAGE."
  (package-refresh-contents)
  (when (el-get-elpa-update-available-p package)
    (el-get-elpa-remove package url nil)
    (package-install (el-get-as-symbol package)))
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
  :install-hook #'el-get-elpa-install-hook
  :remove-hook #'el-get-elpa-remove-hook
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
		   (pkg-desc    (cdr pkg))
		   (description (package-desc-doc pkg-desc))
		   (depends     (mapcar #'car (package-desc-reqs pkg-desc)))
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
		(insert ")")
		(indent-region (point-min) (point-max)))))
	  package-archive-contents)))

(provide 'el-get-elpa)
