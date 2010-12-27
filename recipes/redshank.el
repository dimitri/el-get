(:name redshank
       :type darcs
       :url "http://www.foldr.org/~michaelw/projects/redshank"
       :load-path (".")
       :after (lambda ()
		(require 'redshank-loader)
		(eval-after-load "redshank-loader"
		  `(redshank-setup '(lisp-mode-hook
				     slime-repl-mode-hook) t))))