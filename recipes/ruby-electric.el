(:name ruby-electric
       :description "Electric commands editing for ruby files"
       :type elpa
       :post-init (lambda ()
                    (add-hook 'ruby-mode-hook 'ruby-electric-mode)))
