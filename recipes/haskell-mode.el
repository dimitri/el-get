(:name haskell-mode
       :type darcs
       :url "http://code.haskell.org/haskellmode-emacs"
       :load "haskell-site-file.el"
       :post-init (lambda ()
		    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
		    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
