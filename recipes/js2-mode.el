(:name js2-mode
       :type git
       :url "https://github.com/emacsmirror/js2-mode.git"
       :compile "js2-mode.el"
       :post-init (lambda ()
		    (autoload 'js2-mode "js2-mode" nil t)))
