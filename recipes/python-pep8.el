(:name python-pep8
       :type emacsmirror
       :features python-pep8
       :post-init (lambda ()
		    (require 'tramp)))
