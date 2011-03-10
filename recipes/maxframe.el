(:name maxframe
       :type git
       :url "http://github.com/rmm5t/maxframe.el.git"
       :features maxframe
       :post-init (lambda () (add-hook 'window-setup-hook 'maximize-frame)))
