(:name js2-mode
       :type emacsmirror
       :compile "js2-mode.el"
       :post-init (lambda ()
		    (autoload 'js2-mode "js2-mode" nil t)))
