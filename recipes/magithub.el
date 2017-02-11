(:name magithub
       :type github
       :description "Magit interfaces for GitHub."
       :pkgname "vermiculus/magithub"
       :depends (magit s)
       :features magithub
       :post-init (magithub-feature-autoinject t))
