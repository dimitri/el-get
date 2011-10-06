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
;;     Please see the README.asciidoc file from the same distribution

(require 'cl)				; yes I like loop
(require 'bytecomp)

;; Emacs < 24
(eval-and-compile
  (if (fboundp 'byte-recompile-file)
      (defsubst el-get-byte-compile-file (el)
        ;; Byte-compile runs emacs-lisp-mode-hook; disable it
        (let (emacs-lisp-mode-hook)
          (byte-recompile-file el)))
    (defun el-get-byte-compile-file (el)
      "Same as `byte-compile-file', but skips unnecessary compilation.

Specifically, if the compiled elc file already exists and is
newer, then compilation will be skipped."
      (let ((elc (concat (file-name-sans-extension el) ".elc"))
            ;; Byte-compile runs emacs-lisp-mode-hook; disable it
            emacs-lisp-mode-hook)
        (when (or (not (file-exists-p elc))
                  (file-newer-than-file-p el elc))
          (condition-case err
              (byte-compile-file el)
            ((debug error) ;; catch-all, allow for debugging
             (message "%S" (error-message-string err)))))))))

(defun el-get-byte-compile-file-or-directory (file)
  "Byte-compile FILE or all files within it if it is a directory."
  (let ((byte-compile-warnings nil)
        ;; Byte-compile runs emacs-lisp-mode-hook; disable it
        emacs-lisp-mode-hook)
    (if (file-directory-p file)
        (byte-recompile-directory file 0)
      (el-get-byte-compile-file file))))

(defun el-get-assemble-files-for-byte-compilation (package)
  "Assemble a list of *absolute* paths to byte-compile for PACKAGE."
  (when el-get-byte-compile
    (let* ((source   (el-get-package-def package))
           (comp-prop (plist-get source :compile))
           (compile (el-get-as-list comp-prop))
           ;; nocomp is true only if :compile is explicitly set to nil.
           (explicit-nocomp (and (plist-member source :compile)
                                 (not comp-prop)))
	   (method   (el-get-package-method source))
	   (pdir     (el-get-package-directory package))
	   (el-path  (el-get-load-path package))
	   (files '()))
      (cond
       (compile
        ;; only byte-compile what's in the :compile property of the recipe
        (dolist (path compile)
          (let ((fullpath (expand-file-name path pdir)))
            (if (file-exists-p fullpath)
                ;; path is a file/dir, so add it literally
                (add-to-list 'files fullpath)
              ;; path is a regexp, so add matching file names in package dir
              (mapc (apply-partially 'add-to-list 'files)
		    (directory-files pdir nil fullpath))))))

       ;; If package has (:compile nil), or package has its own build
       ;; instructions, or package is already pre-compiled by the
       ;; installation method, then don't compile anything.
       ((or explicit-nocomp
            (el-get-build-commands package)
            (member method '(apt-get fink pacman)))
        nil)

       ;; Default: compile the package's entire load-path
       (t
        (mapc (apply-partially 'add-to-list 'files) el-path)))
      files)))

(defun el-get-byte-compile-from-stdin ()
  "byte compile files read on STDIN

This is run as a subprocess with an `emacs -Q -batch -f
el-get-byte-compile` command and with the file list as stdin,
written by `prin1-to-string' so that `read' is able to process
it."
  (let ((files (read)))
    (loop for f in files
	  do (progn
	       (message "el-get-byte-compile-from-stdin: %s" f)
	       (el-get-byte-compile-file-or-directory f)))))

(defun el-get-byte-compile-process (package buffer working-dir sync files)
  "return the 'el-get-start-process-list' entry to byte compile PACKAGE"
  (let ((bytecomp-command
	 (list el-get-emacs
	       "-Q" "-batch" "-f" "toggle-debug-on-error"
	       "-l" (file-name-sans-extension
                     (symbol-file 'el-get-byte-compile-from-stdin 'defun))
	       "-f" "el-get-byte-compile-from-stdin")))
    `(:command-name "byte-compile"
		    :buffer-name ,buffer
		    :default-directory ,working-dir
		    :shell t
		    :sync ,sync
		    :stdin ,files
		    :program ,(car bytecomp-command)
		    :args ,(cdr bytecomp-command)
		    :message ,(format "el-get-build %s: byte-compile ok." package)
		    :error ,(format
			     "el-get could not byte-compile %s" package))))

(defun el-get-byte-compile (package)
  "byte compile files for given package"
  (let ((pdir  (el-get-package-directory package))
	(buf   "*el-get-byte-compile*")
	(files (el-get-assemble-files-for-byte-compilation package)))
    (when files
      (el-get-start-process-list
       package
       (list (el-get-byte-compile-process package buf pdir t files))
       nil))))


(provide 'el-get-byte-compile)
