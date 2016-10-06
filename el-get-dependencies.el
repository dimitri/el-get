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
(require 'el-get-recipes)

(eval-and-compile
  (unless (fboundp 'string-suffix-p) ; introduced in 24.4
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case)))))))
(defun el-get-dependencies (packages)
  "Return the list of packages to install in order."
  (multiple-value-bind (plist all-sorted-p non-sorted)
      (topological-sort
       (apply 'append (mapcar 'el-get-dependencies-graph (el-get-as-list packages))))
    (if all-sorted-p
        plist
      (error "Couldn't sort package dependencies for \"%s\"" packages))))

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
  (let* ((entries (make-hash-table :test test))
         ;; avoid obsolete `flet' & backward-incompatible `cl-flet'
         (entry (lambda (v)
                  "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
                  (or (gethash v entries)
                      (puthash v (cons 0 '()) entries)))))
    ;; populate entries initially
    (dolist (gvertex graph)
      (destructuring-bind (vertex &rest dependencies) gvertex
        (let ((ventry (funcall entry vertex)))
          (dolist (dependency dependencies)
            (let ((dentry (funcall entry dependency)))
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
        (let* ((v (pop S)) (ventry (funcall entry v)))
          (remhash v entries)
          (dolist (dependant (cdr ventry) (push v L))
            (when (zerop (decf (car (funcall entry dependant))))
              (push dependant S)))))
      ;; return (1) the list of sorted items, (2) whether all items
      ;; were sorted, and (3) if there were unsorted vertices, the
      ;; hash table mapping these vertices to their dependants
      (let ((all-sorted-p (zerop (hash-table-count entries))))
        (values (nreverse L)
                all-sorted-p
                (unless all-sorted-p
                  entries))))))

(defun el-get-auto-dependencies (package &optional interactive)
  "Return a plist with `:depends' based on the `Package-Requires'
  header in PACKAGE's elisp file(s).

A `:minimum-emacs-version' property may also be present."
  (interactive (list (el-get-read-package-with-status "Auto-get dependencies of" "installed") t))
  (unless (el-get-package-installed-p package)
    (error "Tried to get Package-Requires of non-installed package, `%s'!" package))
  (eval-and-compile
    (require 'lisp-mnt))                ; `lm-header'
  (loop with deps and min-emacs and sub-pkgs
        for pdir in (el-get-load-path package)
        do (dolist (file (directory-files pdir t "\\.el\\'" t))
             (if (string-suffix-p "-pkg.el" file)
                 (let ((def-pkg (el-get-read-from-file file)))
                   (push (intern (nth 1 def-pkg)) sub-pkgs)
                   (setq deps (nconc (el-get-unquote (nth 4 def-pkg)) deps)))
               (with-temp-buffer
                 (insert-file-contents file)
                 (let ((pkg-reqs (lm-header "package-requires")))
                   (when pkg-reqs
                     (push (intern (file-name-base file)) sub-pkgs)
                     (setq deps (nconc (car (read-from-string pkg-reqs)) deps)))))))
        finally do
        (setq min-emacs (car (cdr (assq 'emacs deps)))
              deps (set-difference (remq 'emacs (delete-dups (mapcar #'car deps)))
                                   sub-pkgs))
        (let ((non-el-get-pkgs (remove-if #'el-get-package-def deps)))
          (when non-el-get-pkgs
            (error "Found non el-get package(s): %s" non-el-get-pkgs)))
        finally return
        (if interactive
            (let ((props-str
                   (apply #'concat ":depends " (prin1-to-string deps) "\n"
                          (when min-emacs
                            (list ":minimum-emacs-version " (prin1-to-string min-emacs) "\n")))))
              (message "%s" props-str)
              (kill-new props-str))
          (nconc (if min-emacs (list :minimum-emacs-version min-emacs))
                 (list :depends deps)))))

(defun el-get-auto-update-dependencies (package buffer &optional interactive)
  "Update the dependencies of PACKAGE according to its source headers.

Interactively, update the recipe in the current buffer if it's
visiting a recipe for the chosen PACKAGE, otherwise visit the
corresponding recipe file."
  (interactive (let ((pkg (el-get-read-package-with-status
                           "Auto update dependencies of" "installed")))
                 (list pkg
                       (if (string= (file-name-base buffer-file-name) pkg)
                           (current-buffer) (find-file (el-get-recipe-filename pkg)))
                       t)))
  (with-current-buffer buffer
   (let* ((new-props (el-get-auto-dependencies package))
          (recipe (save-excursion (goto-char (point-min))
                                  (read (current-buffer)))))
     (loop with auto-updated = nil
           for (prop newval) on new-props by #'cddr
           for prop-name = (symbol-name prop)
           unless (equal newval (plist-get recipe prop))
           do (save-excursion
                (goto-char (point-min))
                (let ((have-prop (search-forward prop-name nil t)))
                  (if have-prop (let ((opoint (point)))
                                  (forward-sexp)
                                  (delete-region opoint (point)))
                    (insert prop-name))
                 (insert " ")
                 (prin1 newval (current-buffer))
                 (unless (looking-at-p " ; auto updated")
                   (insert " ; auto updated"))
                 (unless have-prop (insert "\n"))
                 (setq auto-updated t)))
           finally (when interactive
                     (message "Dependencies of %s %s updated." package
                              (if auto-updated "have been" "didn't need to be")))))))

(provide 'el-get-dependencies)
