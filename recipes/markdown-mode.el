(:name markdown-mode
       :type git
       :url "git://jblevins.org/git/markdown-mode.git"
       :features markdown-mode
       :after (lambda ()
		(add-to-list 'auto-mode-alist
			     '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
