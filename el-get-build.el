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
(require 'el-get-byte-compile)
(require 'el-get-recipes)

(declare-function info-initialize "info" nil)
(defvar Info-directory-list)

;; debian uses ginstall-info and it's compatible to fink's install-info on
;; MacOSX, so:
(defvar el-get-install-info (or (executable-find "ginstall-info")
                                (executable-find "install-info")))

(defun el-get-build-commands (package &optional safe-eval system)
  "Return a list of build commands for the named PACKAGE.

The result will either be nil; a list of strings, each one to be
interpreted as a shell command; or a list of lists of
strings, each string representing a single shell argument.

If SAFE-EVAL is non-nil, do not evaluate the recipe's :build
section if it is `unsafep'. This is intended for checking the
recipe without actually executing build instructions written in
ELisp.

If SYSTEM is a string, only check `:buildSYSTEM'.
Otherwise, use `:build/SYSTEM-TYPE' or `:build'."
  (let* ((source     (el-get-package-def package))
         (raw-build-commands
          (if (stringp system) (plist-get source (intern (concat ":build" system)))
            (or (plist-get source (intern (format ":build/%s" system-type)))
                (plist-get source :build))))
         (build-commands
          (if (listp raw-build-commands)
              ;; If the :build property's car is a symbol, assume that it is an
              ;; expression that evaluates to a command list, rather than a
              ;; literal command list.
              (if (symbolp (car raw-build-commands))
                  (let ((default-directory (el-get-package-directory package))
                        (unsafe (and safe-eval (unsafep raw-build-commands))))
                    (if unsafe (throw 'unsafe-build unsafe)
                      (eval raw-build-commands)))
                raw-build-commands)
            (error "build commands for package %s are not a list" package)))
         (flat-build-commands
          ;; Flatten lists, but not strings
          (mapcar (lambda (x) (if (stringp x) x (el-get-flatten x)))
                  build-commands)))

    ;; Verify that each build command is a string or a list of strings
    (let ((invalid-cmds
           (remove-if (lambda (cmd)
                        (or (stringp cmd)
                            (el-get-list-of-strings-p cmd)))
                      flat-build-commands)))
      (when invalid-cmds
        (error "Package %s has invalid build commands: %S" package invalid-cmds)))
    flat-build-commands))

(defun el-get-build-command-program (name)
  "Given the user command name, get the command program to execute.

That will find the program in current $PATH for you, unless given
command name is a relative filename beginning with \"./\", or its
absolute filename obtained with expand-file-name is executable."
  (let ((fullname (expand-file-name name))
        (exe      (executable-find name)))
    (cond ((string-match "^\./" name)   name)
          ((and (file-regular-p fullname)
                (file-executable-p fullname)) fullname)
          (t (or exe name)))))

(defun el-get-build
  (package commands &optional subdir sync post-build-fun installing-info)
  "Run each command from the package directory.

COMMANDS is a list of commands to run in order to build the
package.

The commands are run either synchronously or asynchronously
depending on the SYNC parameter, and can be run from SUBDIR
directory when given.  By default COMMANDS are run from the
package directory as obtained by `el-get-package-directory'.

The function POST-BUILD-FUN will get called after the commands
are all successfully run.  In case of asynchronous building, the
only way to have code running after the build is using this
parameter.

INSTALLING-INFO is t when called from
`el-get-install-or-init-info', as to avoid a nasty infinite
recursion.
"
  (el-get-verbose-message "el-get-build %s" package)
  (let* ((pdir   (el-get-package-directory package))
         (wdir   (if subdir (concat (file-name-as-directory pdir) subdir) pdir))
         (buf    (format "*el-get-build: %s*" package))
         (default-directory (file-name-as-directory wdir))
         (shell-file-name (or (and (eq system-type 'windows-nt)
                                   (executable-find "cmdproxy.exe"))
                              shell-file-name))
         (process-list
          (mapcar (lambda (c)
                    (let* ((split    (cond ((stringp c)
                                            ;; `("sh" "-c" ,c) or equivalent
                                            (prog1 (list shell-file-name
                                                         shell-command-switch
                                                         c)
                                              (when (not (string= c (shell-quote-argument c)))
                                                (warn "Build command %S in package \"%s\" will be shell-interpolated. To bypass shell interpolation, the recipe for \"%s\" should specify build commands as lists of strings instead." c package package))))
                                           ((sequencep c) c)
                                           (t (error "Invalid command: %S" c))))
                           (c        (mapconcat 'identity split " "))
                           (name     (car split))
                           (program  (el-get-build-command-program name))
                           (args     (cdr split)))

                      `(:command-name ,name
                                      :buffer-name ,buf
                                      :default-directory ,wdir
                                      :shell t
                                      :sync ,sync
                                      :program ,program
                                      :args (,@args)
                                      :message ,(format "el-get-build %s: %s ok." package c)
                                      :error ,(format
                                               "el-get could not build %s [%s]" package c))))
                  commands))
         ;; Do byte-compilation after building, if needed
         (byte-compile-then-post-build-fun
          `(lambda (package)
             (let ((bytecomp-files
                    (when el-get-byte-compile
                      (el-get-assemble-files-for-byte-compilation package))))
               ;; The byte-compilation command needs to run even if
               ;; `bytecomp-files' is empty, because it also cleans up
               ;; stale compiled files if it finds any.
               (el-get-start-process-list
                package
                (list (el-get-byte-compile-process package ,buf ,wdir ,sync bytecomp-files))
                #',post-build-fun))))
         ;; unless installing-info, post-build-fun should take care of
         ;; building info too
         (build-info-then-post-build-fun
          (if installing-info byte-compile-then-post-build-fun
            `(lambda (package)
               (el-get-install-or-init-info package 'build)
               (funcall ,byte-compile-then-post-build-fun package)))))
    (el-get-start-process-list
     package process-list build-info-then-post-build-fun)))

(defvar el-get-info-paths-to-add nil
  "A list of paths to add to `Info-directory-list' after info is loaded")

(defun el-get-set-info-path (package infodir-rel)
  (cond ((featurep 'info)
         (info-initialize)) ;; ok because Emacs won't initialize twice
        ((null el-get-info-paths-to-add)
         (eval-after-load 'info
           '(progn (info-initialize) ;; ok because Emacs won't initialize twice
                   (setq Info-directory-list (nconc el-get-info-paths-to-add
                                                    Info-directory-list)
                         el-get-info-paths-to-add nil)))))
  ;; We can't just put this call inside `eval-after-load' because the
  ;; package could be removed before info is loaded (and removing
  ;; elements from `after-load-alist' is a bit too tricky).
  (el-get-add-path-to-list package
                           (if (featurep 'info) 'Info-directory-list
                             'el-get-info-paths-to-add)
                           infodir-rel))

(defun el-get-install-or-init-info (package build-or-init)
  "Call `el-get-install-info' to create the necessary \"dir\"
  file when build-or-init is 'build, or `el-get-set-info-path'
  when build-or-init is 'init "
  (let* ((source   (el-get-package-def package))
         (method   (el-get-package-method source))
         (infodir  (plist-get source :info))
         (pname    (el-get-as-string package))
         (pdir     (el-get-package-directory package)))

    ;; apt-get, pacman and ELPA will set up Info-directory-list
    (unless (member method '(elpa apt-get fink pacman))
      (let* ((infodir-abs-conf (concat pdir infodir))
             (infodir-abs (file-name-as-directory
                           (if (file-directory-p infodir-abs-conf)
                               infodir-abs-conf
                             (file-name-directory infodir-abs-conf))))
             (infodir-rel (if (file-directory-p infodir-abs-conf)
                              infodir
                            (file-name-directory infodir)))
             (info-dir    (concat infodir-abs "dir"))
             (infofile (if (and (file-exists-p infodir-abs-conf)
                                (not (file-directory-p infodir-abs-conf)))
                           infodir-abs-conf
                         (concat infodir-abs pname))))

        (cond
         ((eq build-or-init 'init)
          (when (file-exists-p info-dir)
            (el-get-set-info-path package infodir-rel)))

         ((eq build-or-init 'build)
          ;; rebuild each time asked --- e.g. on update
          (when (and infodir el-get-install-info
                     (file-directory-p infodir-abs)
                     (not (file-exists-p info-dir)))
            (el-get-set-info-path package infodir-rel)
            (el-get-build
             package
             (list (list el-get-install-info
                         (convert-standard-filename
                          (if (string= (substring infofile -5) ".info")
                              infofile
                            (concat infofile ".info")))
                         "dir"))
             infodir-rel t nil t)))
         (t
          (error
           "el-get-install-or-init-info: %s not supported" build-or-init)))))))

(provide 'el-get-build)
