(:name enclose
       :type http
       :url "https://github.com/rejeep/enclose/raw/master/enclose.el"
       :post-init (lambda ()
		    (autoload 'enclose-mode "enclose" nil t)))
