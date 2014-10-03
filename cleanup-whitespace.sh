#!/usr/bin/env sh

SETUP='(progn (add-to-list (quote load-path) ".") (load "el-get")(defun cleanup-whitespace () (message "Cleaning whitespace %s" (buffer-file-name)) (setq indent-tabs-mode nil require-final-newline t) (untabify (point-min) (point-max)) (indent-region (point-min) (point-max)) (delete-trailing-whitespace (point-min) (point-max)) (save-buffer)) (add-hook (quote find-file-hook) (function cleanup-whitespace)) (add-to-list (quote auto-mode-alist) (quote ("\\.[rR][cC][pP]$" . emacs-lisp-mode))))'

find . -name '*.el' -o -name '*.rcp' | xargs -- emacs -Q -l simple -batch --eval "$SETUP"
