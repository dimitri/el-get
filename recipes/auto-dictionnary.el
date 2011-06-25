(:name auto-dictionnary
       :website "http://nschum.de/src/emacs/auto-dictionary/"
       :description "auto-dictionary-mode tries to guess the buffer's text language and adjusts flyspell automatically.  Currently English, German, French, Spanish, Swedish, Slovenian, Hungarian, Romanian and Portuguese are somewhat supported.  Additional languages are easily added."
       :type http
       :url "http://nschum.de/src/emacs/auto-dictionary/auto-dictionary.el"
       :features auto-dictionary
       :post-init (lambda ()
		    (add-hook 'flyspell-mode-hook
			      (lambda () (auto-dictionary-mode 1)))))
