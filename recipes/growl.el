(:name growl
       :type http
       :url "http://edward.oconnor.cx/elisp/growl.el"
       :post-init (lambda ()
		    (autoload 'growl "growl" nil t)))
