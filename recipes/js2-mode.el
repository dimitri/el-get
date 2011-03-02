(:name js2-mode
       :type svn
       :url "http://js2-mode.googlecode.com/svn/trunk/"
       :compile "js2-mode.el"
       :post-init (lambda ()
		    (autoload 'js2-mode "js2-mode" nil t)))
