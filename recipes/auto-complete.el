(:name autocomplete
       :type git
       :url "http://github.com/m2ym/auto-complete.git"
       :load-path "."
       :post-init (lambda ()
		    (require 'auto-complete)
		    (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
		    (require 'auto-complete-config)
		    (ac-config-default)))
