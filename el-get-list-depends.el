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

(require 'cl)
(require 'el-get-core)

;;
;; topological sort, see
;; http://rosettacode.org/wiki/Topological_sort#Common_Lisp
;;

(defun el-get-dependencies (package &optional deps)
  "Return the list of packages (as symbols) on which PACKAGE (a
symbol) depends"
  (let* ((source (el-get-package-def (symbol-name package)))
	 (method (el-get-package-method source))
         (pdeps  (el-get-as-list (plist-get source :depends)))
	 (alldeps
	  ;; Make sure all elpa packages depend on the package `package'.
	  ;; The package `package' is an elpa package, though, so exclude it
	  ;; to avoid a circular dependency.
	  (if (and (not (eq package 'package)) (eq method 'elpa))
	      (cons 'package pdeps)
	    pdeps)))
    (append (list (append (list package) alldeps))
	    (loop for p in pdeps append (el-get-dependencies p)))))

(defun* topological-sort (graph &key (test 'eql))
  "Returns a list of packages to install in order.

  Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (v)
             "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
	     (or (gethash v entries)
		 (puthash v (cons 0 '()) entries))))
      ;; populate entries initially
      (dolist (gvertex graph)
        (destructuring-bind (vertex &rest dependencies) gvertex
          (let ((ventry (entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (entry dependency)))
                (unless (funcall test dependency vertex)
		  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (entry dependant))))
                (push dependant S)))))
      (message "ARF %S" L)
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))
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

(defun el-get-package-initialized-p (package)
  (eq (el-get-package-state package) 'init))

(defun el-get-dependency-installed (package dependency)
  "Install the given PACKAGE (a symbol) iff all its dependencies
are now installed"
  (when (every 'el-get-package-initialized-p
               (el-get-dependencies package))
    (el-get-do-install package)))

(defun el-get-dependency-error (package dependency data)
  "Mark PACKAGE as having failed installation due to a failure to
  install DEPENDENCY, with error information DATA"
  (el-get-mark-failed package (list dependency data)))

(provide 'el-get-depends)
