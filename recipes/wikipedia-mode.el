(:name wikipedia-mode
       :type emacswiki
       :features wikipedia-mode
       :after (lambda ()
                (add-to-list 'auto-mode-alist
                             '("\\.wiki\\.txt\\'" . wikipedia-mode))))
