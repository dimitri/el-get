(:name autofit-frame
       :type emacswiki
       :features autofit-frame
       :post-init (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))
