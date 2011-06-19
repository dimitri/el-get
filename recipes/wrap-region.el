(:name wrap-region
       :type http
       :url "https://github.com/rejeep/wrap-region/raw/master/wrap-region.el"
       :post-init (lambda ()
		    (autoload 'wrap-region-mode "wrap-region" nil t)))
