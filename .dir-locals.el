;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (sentence-end-double-space . t)
  (require-final-newline . t)
  (indent-tabs-mode))
 (emacs-lisp-mode
  (whitespace-style face trailing lines-tail)
  (whitespace-line-column . 80)
  (eval progn
        (add-hook 'before-save-hook 'delete-trailing-whitespace)
        (set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
        (set-face-attribute 'whitespace-tab nil :background "red1" :foreground "yellow" :weight 'bold)
        (require 'whitespace)
        (whitespace-mode 0)
        (whitespace-mode 1))))
