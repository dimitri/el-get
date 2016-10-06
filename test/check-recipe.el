#! /bin/sh
":"; exec ${EMACS:-emacs} -batch -Q -l "$0" "$@" # -*-emacs-lisp-*-

;; Usage:
;;     test/check-recipe.el [-Wno-<warning>...] PATH/TO/RECIPE.rcp
;; See `el-get-check-suppressed-warnings' docstring for possible <warning>s.

(add-to-list 'load-path (expand-file-name
                         ".."
                         (file-name-directory load-file-name)))
(require 'el-get-check)
(el-get-check-recipe-batch)
