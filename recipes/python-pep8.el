(:name python-pep8
       :type git
       :url "https://github.com/emacsmirror/python-pep8.git"
       :features python-pep8
       :post-init (lambda ()
		    (require 'tramp)))
