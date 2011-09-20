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

(require 'el-get-core)

;;
;; Support for tracking package states
;;
(defvar el-get-pkg-state
  (make-hash-table)
  "A hash mapping el-get package name symbols to their installation states")

(defun el-get-package-state (package)
  "Return the installation state of PACKAGE.

- nil indicates that installation of the package has not been requested
- 'installing indicates that the package's installation is in progress
- 'init indicates that the package has been initialized
- ('error . <data>) indicates that there was an installation error"
  (gethash (el-get-as-symbol package) el-get-pkg-state))

(defun el-get-currently-installing-p (package)
  (eq (el-get-package-state package) 'installing))

(defun el-get-currently-installing-packages ()
  "Return the packages that are currently installing"
  (loop
   for pkg being the hash-keys of el-get-pkg-state
   if (el-get-currently-installing-p pkg)
   collect pkg))

(defun el-get-set-package-state (package state)
  "Set the installation state of PACKAGE to STATE"
  (puthash (el-get-as-symbol package) state el-get-pkg-state))

(defun el-get-mark-initialized (package)
  "Record the fact that the given PACKAGE has been initialized."
  (el-get-set-package-state package 'init))
(add-hook 'el-get-post-init-hooks 'el-get-mark-initialized)

(defun el-get-mark-removed (package)
  "Record the fact that the given PACKAGE has been initialized."
  (el-get-set-package-state package nil))
(add-hook 'el-get-post-remove-hooks 'el-get-mark-removed)

(defun el-get-mark-failed (package info)
  "Record the fact that the given PACKAGE has failed to install
for reasons described in INFO."
  (el-get-verbose-message "el-get-mark-failed: %s %s" package info)
  (el-get-set-package-state package `(error ,info)))
(add-hook 'el-get-post-error-hooks 'el-get-mark-failed)


;;
;; generic one-shot event support
;;
(defvar el-get-generic-event-tasks (make-hash-table :test 'equal)
  "A hash mapping event triggers to lists of functions to be called")

(defun el-get-generic-event-occurred (event &optional data)
  "Fire all tasks added for the given EVENT (a hash key), passing DATA."
  (let (tasks)
    (while (setq tasks (gethash event el-get-generic-event-tasks))
      (puthash event (cdr tasks) el-get-generic-event-tasks)
      (ignore-errors (funcall (car tasks) data)))))

(defun el-get-add-generic-event-task (event task)
  "Set up TASK to be called when EVENT (a hash key) occurs."
  (puthash event (cons task (gethash event el-get-generic-event-tasks))
           el-get-generic-event-tasks))

(defun el-get-clear-generic-event-tasks (event)
  "Clear all tasks waiting on EVENT (a hash key)"
  (remhash event el-get-generic-event-tasks))


;;
;; fire events for completion of el-get's init, install, and update
;; phases (and for errors).
;;
(defun el-get-event-id (package action)
  (list (el-get-as-symbol package) (intern (format "el-get-%s" action))))

(defun el-get-event-occurred (package action &optional data)
  "Handle the completion of ACTION on PACKAGE (both symbols),
passing DATA"
  ;; If this action finalizes the package state, first cancel other
  ;; final actions
  (let* ((final-actions '(init error))
         (found (position action final-actions)))
    (when found
      (el-get-clear-generic-event-tasks
       (el-get-event-id package (elt final-actions (- 1 found))))))
  ;; Now fire off the generic event
  (el-get-generic-event-occurred (el-get-event-id package action) data))

;; Install hooks that generate events
(dolist (action '(init install update error))
  (add-hook (intern (format "el-get-post-%s-hooks" action))
            `(lambda (p &optional data) (el-get-event-occurred p ',action data))))

(defun el-get-dependencies (package)
  "Return the list of packages (as symbols) on which PACKAGE (a
symbol) depends"
  (let* ((source (el-get-package-def (symbol-name package)))
	 (method (el-get-package-method source))
         (deps (el-get-as-list (plist-get source :depends))))
    ;; Make sure all elpa packages depend on the package `package'.
    ;; The package `package' is an elpa package, though, so exclude
    ;; it to avoid a circular dependency.
    (if (and (not (eq package 'package)) (eq method 'elpa))
        (cons 'package deps)
      deps)))

(defun el-get-package-initialized-p (package)
  (eq (el-get-package-state package) 'init))

(defun el-get-demand1 (package)
  "Install, if necessary, and init the el-get package given by
PACKAGE, a symbol"
  (let ((p (symbol-name package)))
    (if (string= (el-get-package-status p) "installed")
        (el-get-init p)
      (el-get-do-install p))))

(defun el-get-dependency-installed (package dependency)
  "Install the given PACKAGE (a symbol) iff all its dependencies
are now installed"
  (when (every 'el-get-package-initialized-p
               (el-get-dependencies package))
    (el-get-demand1 package)))

(defun el-get-dependency-error (package dependency data)
  "Mark PACKAGE as having failed installation due to a failure to
  install DEPENDENCY, with error information DATA"
  (el-get-mark-failed package (list dependency data)))

(provide 'el-get-depends)
