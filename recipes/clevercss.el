(:name clevercss
       :type git
       :url "https://github.com/jschaf/CleverCSS-Mode.git"
       :post-init (lambda ()
		    (autoload 'clevercss-mode "clevercss" nil t)
		    (add-to-list 'auto-mode-alist
				 '("\\.ccss\\'" . clevercss-mode))))
