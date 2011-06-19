(:name emacs-textmate
       :type git
       :url "https://github.com/ramblex/emacs-textmate.git"
       :features textmate
       :post-init 'tm/initialize)
