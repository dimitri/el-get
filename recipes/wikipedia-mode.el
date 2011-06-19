(:name wikipedia-mode
       :type emacswiki
       :features wikipedia-mode
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist
				 '("\\.wiki\\.txt\\'" . wikipedia-mode))))
