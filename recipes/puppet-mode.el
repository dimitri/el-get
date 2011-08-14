(:name puppet-mode
       :description "A simple mode for editing puppet manifests"
       :type http
       :url "http://projects.puppetlabs.com/projects/puppet/repository/revisions/master/raw/ext/emacs/puppet-mode.el"
       :after (lambda ()
                (autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests" t)
                (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))))
