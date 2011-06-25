(:name auto-complete
       :website "http://cx4a.org/software/auto-complete/"
       :description "The most intelligent auto-completion extension."
       :type git
       :url "http://github.com/m2ym/auto-complete.git"
       :load-path "."
       :post-init (lambda ()
		    (require 'auto-complete)
		    (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
		    (require 'auto-complete-config)
		    (ac-config-default)))
