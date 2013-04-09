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

(require 'cl)				; yes I like loop
(require 'bytecomp)

;; byte-recompile-file:
;;
;;  - in Emacs23 will not recompile a file when the source is newer than the
;;    bytecode (.elc)
;;
;;  - in Emacs24 has another different and unhelpful behavior:
;;
;;    If the `.elc' file does not exist, normally this function *does not*
;;    compile FILENAME. If ARG is 0, that means compile the file even if it
;;    has never been compiled before.
;;
;; so we just define our own
(defun el-get-byte-compile-file (el)
  "Byte compile the EL file, and skips unnecessary compilation.

Specifically, if the compiled elc file already exists and is
newer, then compilation is skipped."
  (let ((elc (concat (file-name-sans-extension el) ".elc"))
	;; Byte-compile runs emacs-lisp-mode-hook; disable it
	emacs-lisp-mode-hook byte-compile-warnings)
    (when (or (not (file-exists-p elc))
	      (file-newer-than-file-p el elc))
      (condition-case err
	  (byte-compile-file el)
	((debug error) ;; catch-all, allow for debugging
	 (message "%S" (error-message-string err)))))))

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
		    (directory-files pdir nil path))))))

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

(defun el-get-clean-stale-compiled-files (dir &optional recursive)
  "In DIR, delete all elc files older than their corresponding el files.

With optional arg RECURSIVE, do so in all subdirectories as well."
  ;; Process elc files in this dir
  (let ((elc-files (directory-files dir 'full "\\.elc$")))
    (loop for elc in elc-files
          for el = (concat (file-name-sans-extension elc) ".el")
          if (and (file-exists-p elc)
                  (not (file-directory-p elc))
                  (not (file-newer-than-file-p elc el)))
          do (progn
               (message "el-get-byte-compile: Cleaning stale compiled file %s" elc)
               (delete-file elc nil)))
    ;; Process subdirectories recursively
    (when recursive
      (loop for dir in (directory-files dir 'full)
            for localdir = (file-name-nondirectory dir)
            if (file-directory-p dir)
            unless (member localdir '("." ".."
                                  ;; This list of dirs to ignore courtesy of ack
                                  ;; http://betterthangrep.com/
                                  "autom4te.cache" "blib" "_build"
                                  ".bzr" ".cdv" "cover_db" "CVS" "_darcs"
                                  "~.dep" "~.dot" ".git" ".hg" "_MTN"
                                  "~.nib" ".pc" "~.plst" "RCS" "SCCS"
                                  "_sgbak" ".svn"))
            do (el-get-clean-stale-compiled-files dir recursive)))))

(defun el-get-byte-compile-from-stdin ()
  "byte compile files from stdin.

Standard input must be a property list with properties
`:load-path' and `:compile-files', each of which should have a
value that is a list of strings. The variable `load-path' will be
set from the `:load-path' property, and then all the files listed
in `:compile-files' will be byte-compiled.

Standard input can also contain a `:clean-directory' property,
whose value is a directory to be cleared of stale elc files."
  (assert noninteractive nil
          "`el-get-byte-compile-from-stdin' is to be used only with -batch")
  (let* ((input-data (read-minibuffer ""))
         (load-path (append (plist-get input-data :load-path) load-path))
         (files (plist-get input-data :compile-files))
         (dir-to-clean (plist-get input-data :clean-directory)))
    (unless (or dir-to-clean files)
      (warn "Did not get a list of files to byte-compile or a directory to clean. The input may have been corrupted."))
    (when dir-to-clean
      (assert (stringp dir-to-clean) nil
              "The value of `:clean-directory' must be a string.")
      (message "el-get-byte-compile: Cleaning stale compiled files in %s" dir-to-clean)
      (el-get-clean-stale-compiled-files dir-to-clean 'recursive))
    (loop for f in files
          do (progn
               (message "el-get-byte-compile: %s" f)
               (el-get-byte-compile-file-or-directory f)))))

(defun el-get-byte-compile-process (package buffer working-dir sync files)
  "return the `el-get-start-process-list' entry to byte compile PACKAGE"
  (let* ((input-data
          (list :load-path (cons "." load-path)
                :compile-files files
                :clean-directory (el-get-package-directory package)))
         (subprocess-function 'el-get-byte-compile-from-stdin)
         (bytecomp-command
          `(,el-get-emacs
            "-Q" "-batch" "-f" "toggle-debug-on-error"
            "-l" ,(file-name-sans-extension
                   (symbol-file subprocess-function 'defun))
            "-f" ,(symbol-name subprocess-function))))
    `(:command-name "byte-compile"
		    :buffer-name ,buffer
		    :default-directory ,working-dir
		    :shell t
                    :stdin ,input-data
		    :sync ,sync
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
    (el-get-start-process-list
     package
     (list (el-get-byte-compile-process package buf pdir t files))
     nil)))

(provide 'el-get-byte-compile)
