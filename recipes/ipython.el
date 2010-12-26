(:name ipython
       :type http
       :url "http://ipython.scipy.org/dist/ipython.el"
       :features ipython
       :after (lambda ()
                (setq py-python-command "ipython")))
