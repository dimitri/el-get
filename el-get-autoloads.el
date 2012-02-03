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
;;     Please see the README.asciidoc file from the same distribution

(require 'el-get-core)
(require 'autoload)

(defvar el-get-outdated-autoloads nil
  "List of package names whose autoloads are outdated")

(defun el-get-save-and-kill (file)
  "Save and kill all buffers visiting the named FILE"
  (let (buf)
    (while (setq buf (find-buffer-visiting file))
      (with-current-buffer buf
        (save-buffer)
        (kill-buffer)))))

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
  (when (and el-get-generate-autoloads
             (file-exists-p el-get-autoload-file))
    (el-get-verbose-message "el-get: evaluating autoload file")
    (el-get-load-fast el-get-autoload-file)))

(defun el-get-update-autoloads (package)
  "Regenerate, compile, and load any outdated packages' autoloads."

  (message "el-get: updating autoloads for %s" package)

  (let (;; Generating autoloads runs theses hooks; disable then
        fundamental-mode-hook
        prog-mode-hook
        emacs-lisp-mode-hook
        ;; use dynamic scoping to set up our loaddefs file for
        ;; update-directory-autoloads
        (generated-autoload-file el-get-autoload-file))

    ;; make sure we can actually byte-compile it
    (el-get-ensure-byte-compilable-autoload-file generated-autoload-file)

    (when (string= (el-get-package-status package) "installed")
      (apply 'update-directory-autoloads (el-get-load-path package)))

    (el-get-save-and-kill el-get-autoload-file)

    (when (file-exists-p el-get-autoload-file)
      (message "el-get: byte-compiling autoload file")
      (when el-get-byte-compile
        (el-get-byte-compile-file el-get-autoload-file))

      (el-get-eval-autoloads))))

(defconst el-get-load-suffix-regexp
  (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "\\'"))

(defun el-get-remove-autoloads (package)
  "Remove from `el-get-autoload-file' any autoloads associated
with the named PACKAGE"
  (when (file-exists-p el-get-autoload-file)
    (with-temp-buffer ;; empty buffer to trick `autoload-find-destination'
      (let ((generated-autoload-file el-get-autoload-file)
            ;; Generating autoloads runs emacs-lisp-mode-hook; disable it
            emacs-lisp-mode-hook
            (autoload-modified-buffers (list (current-buffer))))
        (dolist (dir (el-get-load-path package))
          (when (file-directory-p dir)
            (dolist (f (directory-files dir t el-get-load-suffix-regexp))
              ;; this will clear out any autoloads associated with the file
              ;; `autoload-find-destination' signature has changed in emacs24.
              (if (> emacs-major-version 23)
                  (autoload-find-destination f (autoload-file-load-name f))
                (autoload-find-destination f)))))))
    (el-get-update-autoloads package)))

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

(provide 'el-get-autoloads)
