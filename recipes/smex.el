(:name smex
       :type git
       :url "http://github.com/nonsequitur/smex.git"
       :features smex
       :post-init (lambda ()
		    (smex-initialize)))

