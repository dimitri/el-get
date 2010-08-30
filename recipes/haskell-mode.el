(:name haskell-mode
       :type http-tar
       :options ("xzf")
       :url "http://projects.haskell.org/haskellmode-emacs/haskell-mode-2.8.0.tar.gz"
       :load "haskell-site-file.el"
       :after (lambda ()
		(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
		(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
