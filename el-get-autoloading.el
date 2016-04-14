;;; el-get.el --- Manage the external elisp bits and pieces you depend upon
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

(require 'cl)
(require 'el-get-core)
(require 'el-get-custom)
(require 'autoload)

(declare-function el-get-package-is-installed "el-get" (package))
(declare-function el-get-byte-compile-file "el-get-byte-compile" (el))
(declare-function el-get-package-def "el-get-recipes" (package))
(declare-function el-get-list-package-names-with-status "el-get-status" (&rest statuses))

(defvar el-get-outdated-autoloads nil
  "List of package names whose autoloads are outdated")

(defun el-get-ensure-byte-compilable-autoload-file (file)
  "If FILE doesn't already exist, create it as a byte-compilable
  autoload file (the default created by autoload.el has a local
  no-byte-compile variable that suppresses byte compilation)."
  ;; If we don't explicitly strip out the no-byte-compile variable,
  ;; autoload.el will create it on demand
  (unless (file-exists-p file)
    (write-region
     (replace-regexp-in-string ";; no-byte-compile: t\n" ""
                               (autoload-rubric file)) nil file)))

(defun el-get-load-fast (file)
  "Load the compiled version of FILE if it exists; else load FILE verbatim"
  (load (file-name-sans-extension file) nil (not el-get-verbose)))

(defun el-get-eval-autoloads ()
  "Evaluate the autoloads from the autoload file."
  (when (and el-get-use-autoloads
             (file-exists-p el-get-autoload-file))
    (el-get-verbose-message "el-get: evaluating autoload file")
    (el-get-load-fast el-get-autoload-file)))

(defvar recentf-exclude)

(defun el-get-update-autoloads (package)
  "Regenerate, compile, and load any outdated packages' autoloads."
  (when (el-get-want-autoloads-p package)
    (message "el-get: updating autoloads for %s" package)

    (let ( ;; Generating autoloads runs theses hooks; disable then
          fundamental-mode-hook
          prog-mode-hook
          emacs-lisp-mode-hook
          ;; use dynamic scoping to set up our loaddefs file for
          ;; update-directory-autoloads
          (generated-autoload-file el-get-autoload-file)
          (visited (find-buffer-visiting el-get-autoload-file))
          ;; .loaddefs.el's buffer name MUST be `el-get-autoload-file'
          ;; or else `autoloads.el' thinks it's a secondary autoload
          ;; file and puts MD5 checksums instead of timestamps.
          (find-file-visit-truename nil)
          (recentf-exclude (cons (regexp-quote el-get-autoload-file)
                                 (bound-and-true-p recentf-exclude))))

      (unless (or (not visited)
                  (equal generated-autoload-file (buffer-file-name visited)))
        ;; Kill .loaddefs.el buffer if it has a different name.
        (kill-buffer visited)
        (setq visited nil))

      ;; make sure we can actually byte-compile it
      (el-get-ensure-byte-compilable-autoload-file generated-autoload-file)

      (when (el-get-package-is-installed package)
        (mapc 'update-directory-autoloads
              (remove-if-not #'file-directory-p
                             (el-get-load-path package)))

        (let ((visiting (find-buffer-visiting el-get-autoload-file)))
          ;; `update-directory-autoloads' leaves file open
          (when (and (not visited) visiting)
            (kill-buffer visiting))))

      (when (file-exists-p el-get-autoload-file)
        (message "el-get: byte-compiling autoload file")
        (when el-get-byte-compile
          (el-get-byte-compile-file el-get-autoload-file))

        (el-get-eval-autoloads)))))

(defconst el-get-autoload-regexp
  (let ((tmp nil))
    (dolist (suf (get-load-suffixes))
      (unless (string-match "\\.elc" suf) (push suf tmp)))
    (concat "^[^=.].*" (regexp-opt tmp t) "\\'"))
  "copied from `update-directory-autoloads'")

(defun el-get-remove-autoloads (package)
  "Remove from `el-get-autoload-file' any autoloads associated
with the named PACKAGE"
  (when (file-exists-p el-get-autoload-file)
    (let* ((files (mapcan (lambda (dir)
                            (when (file-directory-p dir)
                              (directory-files dir t el-get-autoload-regexp)))
                          (el-get-load-path package)))
           (generated-autoload-file el-get-autoload-file)
           (load-names (mapcar #'autoload-file-load-name files))
           (recentf-exclude (cons (regexp-quote el-get-autoload-file)
                                  (bound-and-true-p recentf-exclude)))
           (visited (find-buffer-visiting el-get-autoload-file)))
      (with-current-buffer (or visited (find-file-noselect el-get-autoload-file))
        (widen) (goto-char (point-min))
        (while (search-forward generate-autoload-section-header nil t)
          (when (member (nth 2 (autoload-read-section-header)) load-names)
            ;; We found a matching section, remove it.
            (autoload-remove-section (match-beginning 0)))))
      (el-get-update-autoloads package)
      (let ((visiting (find-buffer-visiting el-get-autoload-file)))
        (when visiting
          (when (buffer-modified-p visiting) ; Save loaddefs, if needed.
            (with-current-buffer visiting (save-buffer)))
          ;; Close loaddefs buffer, if we opened it ourselves.
          (unless visited (kill-buffer visiting)))))))

(defun el-get-want-autoloads-p (package)
  "Return t iff the given PACKAGE should have its autoloads
automatically generated by el-get"
  (let ((source (el-get-package-def package)))
    (or (not (plist-member source :autoloads))
        (eq (plist-get source :autoloads) t))))

(defun el-get-invalidate-autoloads (&optional package)
  "Mark the named PACKAGE as needing new autoloads.  If PACKAGE
is nil, marks all installed packages as needing new autoloads."

  ;; If we're invalidating everything, try to start from a clean slate
  (unless package
    (ignore-errors
      (delete-file el-get-autoload-file)
      (delete-file
       (concat (file-name-sans-extension el-get-autoload-file) ".elc"))))

  (let ((packages
         (if package (list package)
           (el-get-list-package-names-with-status "installed"))))
    (mapc 'el-get-update-autoloads packages)))

(provide 'el-get-autoloading)
