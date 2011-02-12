(:name naquadah-theme
       :type git
       :url "git://git.naquadah.org/naquadah-theme.git"
       :after (lambda ()
		(add-to-list 'custom-theme-load-path pdir)))
