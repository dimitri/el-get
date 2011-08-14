(:name rvm
       :description "Emacs integration for rvm"
       :type git
       :features rvm
       :url "https://github.com/senny/rvm.el.git"
       :compile "rvm.el"
       :post-init (lambda () (rvm-use-default)))
