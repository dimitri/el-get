(:name redshank
       :description "A collection of code-wrangling Emacs macros mostly geared towards Common Lisp, but some are useful for other Lisp dialects, too."
       :type darcs
       :url "http://www.foldr.org/~michaelw/projects/redshank"
       :load-path (".")
       :post-init (lambda ()
            (require 'redshank-loader)
            (eval-after-load "redshank-loader"
              `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))))
