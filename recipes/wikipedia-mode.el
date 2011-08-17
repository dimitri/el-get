(:name wikipedia-mode
       :description "Mode for editing Wikipedia articles off-line"
       :type emacswiki
       :features wikipedia-mode
       :post-init (lambda ()
            (add-to-list 'auto-mode-alist
                 '("\\.wiki\\.txt\\'" . wikipedia-mode))))
