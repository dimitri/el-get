(:name pkgbuild-mode
       :type git
       :url "https://github.com/cdkamat/pkgbuild-mode.git"
       :features pkgbuild-mode
       :post-init (lambda nil
		    (add-to-list (quote auto-mode-alist)
				 (quote ("PKGBUILD$" . pkgbuild-mode)))
		    (add-hook (quote pkgbuild-mode-hook)
			      (quote run-coding-hook))))
