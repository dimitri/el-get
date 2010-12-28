(:name fit-frame
       :type emacswiki
       :features fit-frame
       :after (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))