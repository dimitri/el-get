(:name fit-frame
       :description "Resize a frame. In particular, fit a frame to its buffers."
       :type emacswiki
       :features fit-frame
       :post-init (lambda () (add-hook 'after-make-frame-functions 'fit-frame)))
