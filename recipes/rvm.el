(:name rvm-el
       :type git
       :features rvm
       :url "https://github.com/senny/rvm.el.git"
       :compile "rvm.el"
       :after (lambda () (rvm-use-default)))
