#! /bin/sh
":"; exec ${EMACS:-emacs} -batch -Q -l "$0" "$@" # -*-emacs-lisp-*-

;; Usage:
;;     test/check-recipe.el PATH/TO/RECIPE.rcp

(eval-when-compile (require 'cl))
(add-to-list 'load-path (expand-file-name
                         ".."
                         (file-name-directory load-file-name)))
(require 'el-get)

(loop with sumerror = 0
      for file in command-line-args-left
      for numerror = (el-get-check-recipe file)
      when (/= numerror 0)
      do (with-current-buffer (get-buffer-create "*el-get check recipe*")
           (princ (format "Error in %s\n" file))
           (princ (buffer-string))
           (princ "\n\n"))
      sum numerror into sumerror
      finally (when (/= sumerror 0)
                (error "%s error(s) found in total." sumerror)))
