(:name autofit-frame
       :type emacswiki
       :features autofit-frame
       :after (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))