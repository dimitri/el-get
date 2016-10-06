;;; el-get-core.el --- Manage the external elisp bits and pieces you depend upon
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

;;; Commentary:
;;
;; el-get-core provides basic el-get API, intended for developers of el-get
;; and its methods.  See the methods directory for implementation of them.
;;

(require 'dired)
(require 'cl)            ; needed for `remove-duplicates'
(require 'simple)        ; needed for `apply-partially'
(require 'bytecomp)
(require 'autoload)

(declare-function el-get-package-def "el-get-recipes" (package))
(declare-function el-get-installation-failed "el-get" (package signal-data))

(defun el-get-print-to-string (object &optional pretty)
  "Return string representation of lisp object.

Unlike the Emacs builtin printing functions, this ignores
`print-level' and `print-length', ensuring that as much as
possible the returned string will be a complete representation of
the original object."
  (let (print-level print-length)
    (funcall (if pretty #'pp-to-string #'prin1-to-string)
             object)))

(defun el-get-verbose-message (format &rest arguments)
  (when el-get-verbose (apply 'message format arguments)))

(defmacro el-get-with-errors-as-warnings (prefix &rest body)
  (declare (indent 1) (debug t))
  (let ((error-var (make-symbol "err")))
    `(condition-case ,error-var
         (progn ,@body)
       ((debug error)
        (display-warning 'el-get
                         (concat ,prefix (error-message-string ,error-var))
                         :error)
        nil))))

(defsubst el-get-plist-keys (plist)
  "Return a list of all keys in PLIST.

Duplicates are removed."
  (remove-duplicates
   (loop for (k _) on plist by #'cddr
         collect k)
   :test #'eq))

(defsubst el-get-keyword-name (keyword)
  "Return the name of KEYWORD.

This is equivalent to `symbol-name' but it only works on keywords
and it strips the leading colon.

This raises an error if KEYWORD is not a keyword."
  (or (keywordp keyword)
      (error "Not a keyword: %S" keyword))
  (substring (symbol-name keyword) 1))


;;
;; el-get-methods support, those are like backends.
;;
(defvar el-get-methods nil
  "Register methods that el-get can use to fetch and update a given package.

The methods list is a PLIST, each entry has a method name
property which value is another PLIST, which must contain values
for :install, :install-hook, :update, :remove, :remove-hook
and :checksum properties. Those should be the elisp functions to
call for doing the named package action in the given method.")

(defun el-get-method-defined-p (name)
  "Returns t if NAME is a known el-get install method backend, nil otherwise."
  (and (el-get-method name :install) t))

(defun* el-get-register-method (name &key install update remove
                                     install-hook update-hook remove-hook
                                     compute-checksum guess-website)
  "Register the method for backend NAME, with given functions"
  (let (method-def)
    (loop for required-arg in '(install update remove)
          unless (symbol-value required-arg)
          do (error "Missing required argument: :%s" required-arg)
          do (setq method-def
                   (plist-put method-def
                              (intern (format ":%s" required-arg))
                              (symbol-value required-arg))))
    (loop for optional-arg in '(install-hook update-hook remove-hook
                                             compute-checksum guess-website)
          if (symbol-value optional-arg)
          do (setq method-def
                   (plist-put method-def
                              (intern (format ":%s" optional-arg))
                              (symbol-value optional-arg))))
    (setq el-get-methods (plist-put el-get-methods name method-def))))

(put 'el-get-register-method 'lisp-indent-function
     (get 'prog1 'lisp-indent-function))

(defun* el-get-register-derived-method (name derived-from-name
                                             &rest keys &key &allow-other-keys)
  "Register the method for backend NAME.

Defaults for all optional arguments are taken from
already-defined method DERIVED-FROM-NAME."
  (unless (el-get-method-defined-p derived-from-name)
    (error "Cannot derive new el-get method from unknown method %s" derived-from-name))
  (apply #'el-get-register-method name (append keys (plist-get el-get-methods derived-from-name))))

(put 'el-get-register-derived-method 'lisp-indent-function
     (get 'prog2 'lisp-indent-function))

(defun el-get-register-method-alias (name old-name)
  "Register NAME as an alias for install method OLD-NAME."
  (el-get-register-derived-method name old-name))


;;
;; "Fuzzy" data structure handling
;;
;; In el-get-sources, single elements are often allowed instead of a
;; list, and strings and symbols are often interchangeable.
;; Presumably it's easier for users who don't use the customization
;; interface to write such structures as raw elisp.
;;
;;;  "Fuzzy" data structure conversion utilities
(defun el-get-as-string (obj)
  "Return OBJ as a string."
  (cond
   ((stringp obj) obj)
   ((symbolp obj) (symbol-name obj))
   ((numberp obj) (number-to-string obj))
   (t (error "Can't convert %S to string." obj))))

(defun el-get-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defun el-get-as-list (element-or-list)
  "If ELEMENT-OR-LIST is already a list, return it.  Otherwise
returning a list that contains it (and only it)."
  (if (listp element-or-list) element-or-list
    (list element-or-list)))

(defun el-get-list-of-strings-p (obj)
  (or (null obj)
      (and (consp obj)
           (stringp (car obj))
           (el-get-list-of-strings-p (cdr obj)))))

(defun el-get-source-name (source)
  "Return the package name (stringp) given an `el-get-sources'
entry."
  (if (and source (listp source))
      (format "%s" (or (plist-get source :name)
                       (error "Source does not have a :name property: %S" source)))
    (symbol-name source)))


;;
;; Common support bits
;;
(defun el-get-rmdir (package url post-remove-fun)
  "Just rm -rf the package directory. If it is a symlink, delete it."
  (let* ((edir (expand-file-name el-get-dir))
         (pdir (expand-file-name "." (el-get-package-directory package))))
    ;; check that we're all set
    (when (or (string= edir pdir)    ; package is "", or such like
              ;; error if pdir is not a subdirectory of el-get-dir
              (not (string= edir (substring pdir 0 (length edir)))))
      (error "el-get-rmdir: directory '%s' of package '%s' is not inside `el-get-dir' ('%s')."
             pdir package el-get-dir))
    (cond ((file-symlink-p pdir)
           (delete-file pdir))
          ((file-directory-p pdir)
           (delete-directory pdir 'recursive))
          ((file-exists-p pdir)
           (delete-file pdir)))
    (when post-remove-fun
      (funcall post-remove-fun package))))

(defconst el-get-no-shell-quote "\\`[-,./_[:alnum:]]+\\'"
  "Regular expression matching arguments that don't shell quoting.")

(defun el-get-shell-quote-program (program-name)
  "Like `shell-quote-argument' but needs special treatment on Windows."
  (or (when (string-match-p el-get-no-shell-quote program-name) program-name)
      (when (fboundp 'w32-short-file-name)
        ;; If program is really a bat file, putting double quotes around
        ;; it will lead to problems if subsequent arguments are also
        ;; quoted. Use the short 8.3 name instead of quoting. See
        ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18745 for
        ;; details.
        (let (exe (executable-find program-name))
          (when exe (w32-short-file-name exe))))
      (shell-quote-argument program-name)))

(defun el-get-maybe-shell-quote-argument (arg)
  "`shell-quote-argument', if necessary."
  (if (string-match-p el-get-no-shell-quote arg) arg
    (shell-quote-argument arg)))

(defun el-get-read-from-file (filename)
  "Read given FILENAME and return its content (a valid sexp is expected)."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    ((debug error)
     (error "Error reading file %s: %S" filename err))))

(defun el-get-package-is-installed (package)
  "Return true if PACKAGE is installed"
  (and (file-directory-p (el-get-package-directory package))
       (string= "installed"
                (el-get-read-package-status package))))

(defalias 'el-get-package-installed-p #'el-get-package-is-installed)


;;
;; Some tools
;;
(defun el-get-duplicates (list)
  "Return duplicates found in list."
  (loop with dups and once
        for elt in list
        if (member elt once) collect elt into dups
        else collect elt into once
        finally return dups))

(defun el-get-flatten (arg)
  "Return a version of ARG as a one-level list

 (el-get-flatten 'x) => '(x)
 (el-get-flatten '(a (b c (d)) e)) => '(a b c d e)"
  (if (listp arg)
      (apply 'append (mapcar 'el-get-flatten arg))
    (list arg)))

(defun el-get-unquote (arg)
  "Remote quote from ARG, if there is one."
  (if (eq (car arg) 'quote) (nth 1 arg) arg))

(defun el-get-load-path (package)
  "Return the list of absolute directory names to be added to
`load-path' by the named PACKAGE."
  (let* ((source   (el-get-package-def package))
         (el-path  (if (plist-member source :load-path)
                       (el-get-flatten (plist-get source :load-path))
                     '(".")))
         (pkg-dir (el-get-package-directory package)))
    (mapcar (lambda (p) (expand-file-name p pkg-dir)) el-path)))

(defun el-get-method (method-name action)
  "Return the function to call for doing action (e.g. install) in
given method."
  (let* ((method  (if (keywordp method-name) method-name
                    (intern (concat ":" (format "%s" method-name)))))
         (actions (plist-get el-get-methods method)))
    (assert actions nil
            "Unknown recipe type: %s" method)
    (plist-get actions action)))

(defun el-get-check-init ()
  "Check that we can run el-get."
  (unless (file-directory-p el-get-dir)
    (make-directory el-get-dir)))

(defun el-get-package-directory (package)
  "Return the absolute directory name of the named PACKAGE."
  (file-name-as-directory
   (expand-file-name (el-get-as-string package)
                     (expand-file-name el-get-dir))))

(defun el-get-add-path-to-list (package list path)
  "(add-to-list LIST PATH) checking for path existence within
given package directory."
  (let* ((pdir     (el-get-package-directory package))
         (fullpath (expand-file-name (or path ".") pdir)))
    (unless (file-directory-p fullpath)
      (error "el-get could not find directory `%s' for package %s, at %s"
             path package fullpath))
    (add-to-list list fullpath)))

(defun el-get-package-exists-p (package)
  "Return true only when the given package name is either a
directory or a symlink in el-get-dir."
  (let ((pdir (el-get-package-directory package)))
    ;; seems overkill as file-directory-p will always be true
    (or (file-directory-p pdir)
        (file-symlink-p   pdir))))

(defun el-get-url-host (url)
  "Extract host from given URL.

Earlier we used the built-in library `url-parse' to extract host. This broke
installation of CEDET since it requires that the built-in versions of certain
packages (one of them is `eieio') are not loaded before loading it. However
`url-parse' depends on `auth-source' which in turn depends on `eieio' leading to
loading of `eieio' before initializing CEDET causing CEDET's initialization to
fail."
  (string-match "://\\([^/:]+\\)" url)
  (match-string-no-properties 1 url))


;;
;; el-get-reload API functions
;;
(defun el-get-package-files (pdir)
  "Return a list of files loaded from directory PDIR."
  (loop with regexp = (format "^%s" (regexp-quote (file-name-as-directory (file-truename pdir))))
        for (f . nil) in load-history
        when (and (stringp f) (string-match-p regexp (file-truename f)))
        collect (if (string-match-p "\\.elc?$" f)
                    (file-name-sans-extension f)
                  f)))

(defun el-get-package-features (pdir)
  "Return a list of features provided by files in PDIR."
  (loop with regexp = (format "^%s" (regexp-quote (file-name-as-directory (expand-file-name pdir))))
        for (f . l) in load-history
        when (and (stringp f) (string-match-p regexp (file-truename f)))
        nconc (loop for i in l
                    when (and (consp i) (eq (car i) 'provide))
                    collect (cdr i))))


;;
;; call-process-list utility
;;
(defun el-get-start-process-list-sentinel (proc change)
  "When proc has exited and was successful, chain next command."
  (when (eq (process-status proc) 'exit)
    (condition-case err
        (let ((status  (process-exit-status proc))
              (cname   (process-get proc :command-name))
              (cbuf    (process-get proc :buffer-name))
              (message (process-get proc :message))
              (errorm  (process-get proc :error))
              (package (process-get proc :el-get-package))
              (final-f (process-get proc :el-get-final-func))
              (next    (process-get proc :el-get-start-process-list))
              (el-get-sources (process-get proc :el-get-sources)))
          (if (not (eq 0 status))
              (progn
                (when (process-buffer proc)
                  (set-window-buffer (selected-window) cbuf))
                (error "el-get: %s %s" cname errorm))
            (message "el-get: %s" message))

          (when cbuf (kill-buffer cbuf))
          (if next
              (el-get-start-process-list package next final-f)
            (when (functionp final-f)
              (funcall final-f package))))
      ((debug error)
       (el-get-installation-failed (process-get proc :el-get-package) err)))))

(defvar el-get-default-process-sync nil
  "Non-nil value asks `el-get-start-process-list' to run current
process synchronously. Can be overridden by :sync property in
commands argument of `el-get-start-process-list'")

(defun el-get-start-process-list (package commands final-func)
  "Run each command one after the other, in order, stopping at
first error.

Commands should be a list of plists with at least the following
properties:

:default-directory

   default-directory from where to start the command

:command-name

   Name of the command to start, gives the name of the Emacs subprocess.

:buffer-name

   Name of the buffer associated with the command.

:process-filter

   Function to use as a process filter.

:shell

   When set to a non-nil value, use start-process-shell-command
   rather than the default start-process.

:program

   The program to start

:args

   The list of arguments for the program to start

:message

   The message to send upon success

:error

   The error to send upon failure

:sync

   When set to non-nil value, run synchronously.

:stdin

   Standard input to use for the process.  A lisp value is
   expected, it will get `prin1-to-string' then either saved to a
   file for a synchronous process or sent with
   `process-send-string' for an asynchronous one.

Any other property will get put into the process object.

Any element of commands that is nil will simply be ignored. This
makes it easier to conditionally splice a command into the list.
"
  ;; Skip nil elements of commands. This makes it easier for methods
  ;; to conditionally splice commands into the list.
  (while (and commands (null (car commands)))
    (setq commands (cdr commands)))
  (condition-case err
      (if commands
          (let* ((c       (car commands))
                 (next    (cdr commands))
                 (cdir    (plist-get c :default-directory))
                 (cname   (plist-get c :command-name))
                 (cbuf    (plist-get c :buffer-name))
                 (killed  (when (get-buffer cbuf) (kill-buffer cbuf)))
                 (filter  (plist-get c :process-filter))
                 (shell   (plist-get c :shell))
                 (program (if shell
                              (el-get-shell-quote-program (plist-get c :program))
                            (plist-get c :program)))
                 (args    (if shell
                              (mapcar #'el-get-maybe-shell-quote-argument (plist-get c :args))
                            (plist-get c :args)))
                 (sync    (el-get-plist-get-with-default c :sync
                            el-get-default-process-sync))
                 (stdin   (plist-get c :stdin))
                 (default-directory (if cdir
                                        (file-name-as-directory
                                         (expand-file-name cdir))
                                      default-directory)))
            (unless program (error "el-get: :program argument cannot be nil"))
            (if sync
                (progn
                  (el-get-verbose-message "Running commands synchronously: %S" commands)
                  (let* ((startf (if shell #'call-process-shell-command #'call-process))
                         (infile (when stdin (make-temp-file "el-get")))
                         (dummy  (when infile
                                   (with-temp-file infile
                                     (insert (el-get-print-to-string stdin)))))
                         (dummy  (message "el-get is waiting for %S to complete" cname))
                         (status (apply startf program infile cbuf t args))
                         (message (plist-get c :message))
                         (errorm  (plist-get c :error)))
                    (when el-get-verbose
                      (message "%S" (with-current-buffer cbuf (buffer-string))))
                    (if (eq 0 status)
                        (message "el-get: %s" message)
                      (set-window-buffer (selected-window) cbuf)
                      (error "el-get: %s %s" cname errorm))
                    (when infile (delete-file infile))
                    (when cbuf (kill-buffer cbuf))
                    (if next
                        ;; Prevent stack overflow on very long command
                        ;; lists. This allows
                        ;; `el-get-start-process-list' (but not other
                        ;; functions) to recurse indefinitely.
                        (let ((max-specpdl-size (+ 100 max-specpdl-size)))
                          (el-get-start-process-list package next final-func))
                      (when (functionp final-func)
                        (funcall final-func package)))))
              ;; async case
              (el-get-verbose-message "Running commands asynchronously: %S" commands)
              (let* ((startf (if shell #'start-process-shell-command #'start-process))
                     (process-connection-type nil) ; pipe, don't pretend we're a pty
                     (proc (apply startf cname cbuf program args)))
                ;; add the properties to the process, then set the sentinel
                (mapc (lambda (x) (process-put proc x (plist-get c x))) c)
                (process-put proc :el-get-sources el-get-sources)
                (process-put proc :el-get-package package)
                (process-put proc :el-get-final-func final-func)
                (process-put proc :el-get-start-process-list next)
                (when stdin
                  (process-send-string proc (el-get-print-to-string stdin))
                  (process-send-eof proc))
                (set-process-sentinel proc 'el-get-start-process-list-sentinel)
                (when filter (set-process-filter proc filter)))))
        ;; no commands, still run the final-func
        (when (functionp final-func)
          (funcall final-func package)))
    ((debug error)
     (el-get-installation-failed package err))))

;;
;; get an executable given its command name, with friendly error message
;;
(defun el-get-executable-find (name)
  "Return the absolute path of the command to execute, and errors
out if that can not be found.

This function will first look for existing function named
\"el-get-NAME-executable\" and call that. This function, if it
exists, must handle error cases.

Then, it will look for existing variable named \"el-get-NAME\"
and error if that's not nil and not an existing file name.

Baring variable named \"el-get-NAME\", it will call
`executable-find' on NAME and use the output of that, or error
out if it's nil."
  (let ((fname (intern (format "el-get-%s-executable" name)))
        (vname (intern (format "el-get-%s" name))))
    (cond
     ((fboundp fname)
      (funcall fname))

     ;; vname is bound here, we want to check for the variable named vname
     ;; (bound-and-true-p vname) won't cut it
     ((ignore-errors (symbol-value vname))
      (let ((command (symbol-value vname)))
        (unless (and (file-exists-p command)
                     (file-executable-p command))
          (error
           (concat "The variable `%s' points to \"%s\", "
                   "which is not an executable file name on your system.")
           name command))
        command))

     (t
      (let ((command (executable-find name)))
        (unless command
          (error
           "The command named '%s' can not be found with `executable-find'"
           name))
        command)))))

(defun el-get-plist-get-with-default (plist prop def)
  "Same as (plist-get PLIST PROP), but falls back to DEF.

Specifically, if (plist-member PLIST PROP) is nil, then returns
DEF instead. Note that having a property set to nil is not the
same as having it unset."
  (if (plist-member plist prop)
      (plist-get plist prop)
    def))
(put 'el-get-plist-get-with-default 'lisp-indent-function
     (get 'prog2 'lisp-indent-function))

(provide 'el-get-core)
