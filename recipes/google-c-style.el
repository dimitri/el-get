(:name google-c-style :type http
       :url "http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el"
       :features google-c-style
       :post-init (lambda()
                    (add-hook 'c-mode-common-hook 'google-set-c-style)
                    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))
