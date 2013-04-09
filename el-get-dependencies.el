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

(require 'cl)
(require 'el-get-core)

(defun el-get-dependencies (packages)
  "Return the list of packages to install in order."
  (multiple-value-bind (plist all-sorted-p non-sorted)
      (topological-sort
       (apply 'append (mapcar 'el-get-dependencies-graph (el-get-as-list packages))))
    (if all-sorted-p
	plist
      (error "Couldn't sort package dependencies for \"%s\"" package))))

(defun el-get-dependencies-graph (package)
  "Return the graph of packages on which PACKAGE depends"
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
	    (loop for p in pdeps append (el-get-dependencies-graph p)))))

;;
;; topological sort, see
;; http://rosettacode.org/wiki/Topological_sort#Common_Lisp
;;
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
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))

(provide 'el-get-dependencies)
