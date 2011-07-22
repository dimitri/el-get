(:name ipython
       :description "Adds support for IPython to python-mode.el"
       :type http
       :url "http://ipython.scipy.org/dist/ipython.el"
       :features ipython
       :post-init (lambda ()
            (setq py-python-command "ipython")))
