(:name ipython
       :type http
       :url "http://ipython.scipy.org/dist/ipython.el"
       :features ipython
       :post-init (lambda ()
		    (setq py-python-command "ipython")))
