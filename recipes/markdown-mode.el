(:name markdown-mode
       :description "Major mode to edit Markdown files in Emacs"
       :type git
       :url "git://jblevins.org/git/markdown-mode.git"
       :features markdown-mode
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist
				 '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
