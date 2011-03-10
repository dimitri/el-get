(:name fit-frame
       :type emacswiki
       :features fit-frame
       :post-init (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))
