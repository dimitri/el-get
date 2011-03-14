(:name yaml-mode
       :type git
       :url "https://github.com/yoshiki/yaml-mode.git"
       :post-init (lambda ()
		    (autoload 'yaml-mode "yaml-mode" nil t)
		    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))))
