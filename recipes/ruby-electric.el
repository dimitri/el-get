(:name ruby-electric
       :type elpa
       :post-init (lambda ()
                    (add-hook 'ruby-mode-hook 'ruby-electric-mode)))
