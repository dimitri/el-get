(:name auto-dictionnary
       :type http
       :url "http://nschum.de/src/emacs/auto-dictionary/auto-dictionary.el"
       :features auto-dictionary
       :post-init (lambda ()
		    (add-hook 'flyspell-mode-hook
			      (lambda () (auto-dictionary-mode 1)))))
