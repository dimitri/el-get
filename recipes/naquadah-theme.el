(:name naquadah-theme
       :type git
       :url "git://git.naquadah.org/naquadah-theme.git"
       :post-init (lambda ()
		    (add-to-list 'custom-theme-load-path pdir)))
