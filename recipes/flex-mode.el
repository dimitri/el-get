(:name flex-mode
       :type http
       :url "http://ftp.sunet.se/pub/gnu/emacs-lisp/incoming/flex-mode.el"
       :features flex-mode
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))))
