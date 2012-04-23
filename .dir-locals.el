;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (sentence-end-double-space . t)
  (require-final-newline . t)
  (indent-tabs-mode))
 (emacs-lisp-mode
  (whitespace-style face trailing lines-tail)
  (whitespace-line-column . 80)
  (eval ignore-errors
        "Write-contents-functions is a buffer-local alternative to before-save-hook"
        (add-hook 'write-contents-functions
                  (lambda ()
                    (delete-trailing-whitespace)
                    nil))
        (require 'whitespace)
        "Sometimes the mode needs to be toggled off and on."
        (whitespace-mode 0)
        (whitespace-mode 1))))
