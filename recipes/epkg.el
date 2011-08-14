(:name epkg
       :website "https://github.com/tarsius/epkg"
       :description "The stupid package tracker for Emacs"
       :type git
       :url "https://github.com/tarsius/epkg.git"
       :depends (vcomp magit listing)
       :compile ("epkg.el" "epkg-ui.el"))