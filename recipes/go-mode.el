(:name go-mode
       :type http
       :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip"
       :localname "go-mode.el"
       :features go-mode
       :post-init (lambda ()
		    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))))
