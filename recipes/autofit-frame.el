(:name autofit-frame
       :type emacswiki
       :after (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))